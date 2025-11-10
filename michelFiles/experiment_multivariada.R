## ================== CONFIG INICIAL ==================
# devtools::install_github("Michelcyc/harbinger@minha-trilha-nova", force = TRUE)
suppressPackageStartupMessages({
  library(daltoolbox)
  library(daltoolboxdp)
  library(tspredit)
  library(harbinger)
  library(united)
})

# --- utils já usados antes ---
safe_get <- function(lst, key) {
  if (!length(lst)) return(NULL)
  if (is.null(names(lst))) return(NULL)
  lst[[key]]
}

atomic_save <- function(x, path, compress = "xz") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  nm  <- deparse(substitute(x))
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  save(list = nm, file = tmp, compress = compress)
  ok <- file.rename(tmp, path)
  if (!ok) {
    ok2 <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
    if (!ok2) stop("Falha ao gravar cache em: ", path)
  }
}

count_true <- function(x) {
  if (is.null(x)) return(NA_integer_)
  if (is.data.frame(x)) {
    if ("event" %in% names(x)) return(sum(as.logical(x$event), na.rm = TRUE))
    return(NA_integer_)
  }
  sum(as.logical(x), na.rm = TRUE)
}

get1 <- function(lst, name, default = NA) {
  val <- lst[[name]]
  if (is.null(val) || length(val) == 0) return(default)
  val[[1]]
}

## ================== MODELOS ==================
metodos <- list(
  hanr_fbiad(),   # Método 1
  hanr_arima()    # Método 2
)
names(metodos) <- c("fbiad", "arima")

## ================== DADOS (ex.: Oil_3w) ==================

dataset_name <- "oil_3w_Type_2"
data(list = dataset_name)
data <- get(dataset_name)
nome_base <- dataset_name
stopifnot(exists("data"))

# 'data' esperado: list de data.frames multivariados, cada um com 'event' e várias variáveis numéricas.

# Lista de séries
series_ts <- data
if (!is.list(series_ts) || is.data.frame(series_ts)) {
  stop("'data' precisa ser uma lista de data.frames (cada série multivariada).")
}

# descobre, para cada série, quais colunas serão tratadas como 'variáveis' univariadas
colunas_variaveis_por_serie <- lapply(series_ts, function(df) {
  if (!is.data.frame(df)) return(character(0))
  # exclui colunas não-observacionais comuns:
  drop <- c("event", "idx", "type", "time", "timestamp", "date", "datetime")
  vars <- setdiff(names(df), drop)
  # mantém apenas numéricas
  vars[vapply(df[vars], is.numeric, logical(1))]
})

## ================== CACHE E DETECÇÃO ==================
dir.create("results", showWarnings = FALSE, recursive = TRUE)

detalhes_todos <- list()

for (j in seq_along(metodos)) {
  modelo_atual    <- metodos[[j]]
  nome_modelo     <- names(metodos)[j]
  detalhes_modelo <- list()

  # cache por dataset + método
  arq_cache <- file.path("results", sprintf("exp_detail_%s_%s_mv.RData", nome_base, nome_modelo))

  if (file.exists(arq_cache)) {
    load(file = arq_cache)
    print("Carreguei dados do cache")
    # carrega 'detalhes_modelo' se existir
  }

  # percorre todas as (série, variável)
  for (i in seq_along(series_ts)) {
    dados_multi  <- series_ts[[i]]
    nome_serie   <- names(series_ts)[i]
    vars_i       <- colunas_variaveis_por_serie[[i]]

    if (length(vars_i) == 0L) {
      message(sprintf("Aviso: série '%s' não tem variáveis numéricas utilizáveis.", nome_serie))
      next
    }

    for (var in vars_i) {
      key <- sprintf("%s::%s", nome_serie, var)

      if (!is.null(safe_get(detalhes_modelo, key))) {
        next  # já processado
      }

      detalhes_modelo[[key]] <- tryCatch({
        x <- as.numeric(dados_multi[[var]])
        if (anyNA(x)) x <- replace(x, is.na(x), 0) # estratégia simples; ajuste se quiser

        # ground-truth (se não houver event, vira FALSE)
        y_event <- if ("event" %in% names(dados_multi)) {
          as.logical(dados_multi$event)
        } else {
          rep(FALSE, length(x))
        }

        # 1) fit
        t0 <- Sys.time()
        md <- fit(modelo_atual, x)
        time_fit <- as.double(Sys.time() - t0, units = "secs")

        # 2) detect
        t0 <- Sys.time()
        rs <- detect(md, x)
        time_detect <- as.double(Sys.time() - t0, units = "secs")

        # pacote do resultado
        list(
          md          = md,
          rs          = rs,
          dataref     = i,                # índice da série
          varname     = var,              # qual variável foi usada
          modelname   = nome_modelo,
          datasetname = nome_base,
          seriesname  = nome_serie,
          time_fit    = time_fit,
          time_detect = time_detect
        )
      }, error = function(e) {
        message(sprintf("Erro em %s - %s$%s: %s", nome_modelo, nome_serie, var, e$message))
        NULL
      })

      # salva incrementalmente
      atomic_save(detalhes_modelo, arq_cache, compress = "xz")
    }
  }

  detalhes_todos <- c(detalhes_todos, detalhes_modelo)
}

## ================== SUMÁRIO ==================
linhas_resumo <- vector("list", length(detalhes_todos))

for (k in seq_along(detalhes_todos)) {
  exp_k <- detalhes_todos[[k]]
  if (is.null(exp_k)) next

  dados_k <- series_ts[[exp_k$dataref]]
  tam_serie <- nrow(dados_k)

  num_eventos   <- if ("event" %in% names(dados_k)) count_true(dados_k$event) else NA_integer_
  num_deteccoes <- count_true(exp_k$rs$event)

  # avaliação soft
  t0 <- Sys.time()
  avaliacao_soft <- evaluate(
    har_eval_soft(sw_size = 10),
    exp_k$rs$event,
    if ("event" %in% names(dados_k)) dados_k$event else rep(FALSE, tam_serie)
  )
  tempo_metrica <- as.double(Sys.time() - t0, units = "secs")

  n_cases_simple   <- as.integer(get1(avaliacao_soft, "n_cases_simple",  NA_integer_))
  n_cases_medium   <- as.integer(get1(avaliacao_soft, "n_cases_medium",  NA_integer_))
  n_cases_complex  <- as.integer(get1(avaliacao_soft, "n_cases_complex", NA_integer_))
  sigma_max_de3    <- as.numeric(get1(avaliacao_soft, "sigma_max_de3",   NA_real_))

  linear_behavior <- !is.na(sigma_max_de3) &&
    !is.na(num_eventos) &&
    !is.na(num_deteccoes) &&
    sigma_max_de3 < (num_eventos + num_deteccoes)

  linhas_resumo[[k]] <- data.frame(
    method          = exp_k$modelname,
    dataset         = exp_k$datasetname,
    series          = exp_k$seriesname,
    variable        = exp_k$varname,     # <=== NOVA COLUNA
    time_fit        = exp_k$time_fit,
    time_detect     = exp_k$time_detect,
    time_metric     = tempo_metrica,
    precision       = get1(avaliacao_soft, "precision", NA_real_),
    recall          = get1(avaliacao_soft, "recall",    NA_real_),
    f1              = get1(avaliacao_soft, "F1",        NA_real_),
    tam_serie       = tam_serie,
    num_eventos     = num_eventos,
    num_deteccoes   = num_deteccoes,
    n_cases_simple  = n_cases_simple,
    n_cases_medium  = n_cases_medium,
    n_cases_complex = n_cases_complex,
    sigma_max_de3   = sigma_max_de3,
    linear_behavior = linear_behavior,
    stringsAsFactors = FALSE
  )
}

resumo_experimentos <- do.call(rbind, linhas_resumo)

## ================== SALVA RESUMO ==================
save(resumo_experimentos,
     file = file.path("results", sprintf("exp_summary_%s_mv.RData", nome_base)),
     compress = "xz")

# EXTRA
total_tempo_metric <- sum(resumo_experimentos$time_metric, na.rm = TRUE)
print(total_tempo_metric)
