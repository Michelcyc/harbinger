remove.packages("harbinger")
quit(save = "no")

install.packages('RcppHungarian')
library(RcppHungarian)

devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")

# Pacotes necessários
library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(harbinger)
library(united)

# depois de library(daltoolbox) e library(daltoolboxdp)
if (!("ts_data" %in% getNamespaceExports("daltoolbox")) &&
    exists("ts_data", where = asNamespace("daltoolboxdp"), inherits = FALSE)) {

  ns <- asNamespace("daltoolbox")

  # 1) injeta o objeto na namespace
  assignInNamespace("ts_data", daltoolboxdp::ts_data, ns = "daltoolbox")

  # 2) adiciona à tabela de exports (para :: funcionar)
  unlockBinding(".__NAMESPACE__.", ns)
  ns$.__NAMESPACE__.$exports <- unique(c(ns$.__NAMESPACE__.$exports, "ts_data"))
  lockBinding(".__NAMESPACE__.", ns)
}

safe_get <- function(lst, i) {
  if (i > 0 && i <= length(lst)) {
    lst[[i]]
  } else {
    NULL
  }
}

# --- SAVE ATÔMICO (adição mínima) -------------------------------------------
atomic_save <- function(x, path, compress = "xz") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  nm  <- deparse(substitute(x))                         # salva com o mesmo nome do objeto
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  save(list = nm, file = tmp, compress = compress)      # escreve no tmp primeiro
  ok <- file.rename(tmp, path)                          # move atômico no mesmo FS
  if (!ok) {
    # fallback (ex.: FS diferente); garante que não fica 0 bytes
    ok2 <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
    if (!ok2) stop("Falha ao gravar cache em: ", path)
  }
}
# ---------------------------------------------------------------------------

## ------------------------------------------------------------
## 1) Preparação dos métodos (modelos) ----
## ------------------------------------------------------------
metodos <- list(
  hanr_fbiad(),  # Método 1: FBIAD
  hanr_arima()   # Método 2: ARIMA
)
names(metodos) <- c("fbiad", "arima")

## ------------------------------------------------------------
## 2) Preparação dos dados ----
## ------------------------------------------------------------
nome_base <- "gecco"
data(gecco)
#ds <- gecco  # alias

series_ts <- vector("list", length(gecco))
for (i in seq_along(series_ts)) {
  serie_nome <- names(gecco)[i]
  n <- nrow(gecco[[i]])
  if (is.null(n)) stop(sprintf("Objeto %s não é um data.frame/ts esperado.", serie_nome))
  series_ts[[i]] <- gecco[[i]]
  names(series_ts)[i] <- serie_nome
}


## Garante diretório de resultados
dir.create("results", showWarnings = FALSE, recursive = TRUE)

## ------------------------------------------------------------
## 3) Detecção detalhada (com cache por método) ----
## ------------------------------------------------------------
detalhes_todos <- list()

for (j in seq_along(metodos)) {
  modelo_atual   <- metodos[[j]]
  nome_modelo    <- names(metodos)[j]
  detalhes_modelo <- list()

  arq_cache <- file.path("results", sprintf("exp_detail_%s.RData", nome_modelo))

  if (file.exists(arq_cache)) {
    load(file = arq_cache)  # cria 'detalhes_modelo'
    # >>> Se já existe cache, só acumula e VAI para o próximo método
    detalhes_todos <- c(detalhes_todos, detalhes_modelo)
    next
  }

  # >>> Se NÃO existe cache, aí sim calcula e salva:
  for (i in seq_along(series_ts)) {
    dados_serie <- series_ts[[i]]
    nome_serie  <- names(series_ts)[i]

    result <- safe_get(detalhes_modelo, i)
    if (is.null(result)) {
      detalhes_modelo[[i]] <- tryCatch({
        inicio_tempo <- Sys.time()
        modelo_ajustado <- fit(modelo_atual, dados_serie$value)
        tempo_ajuste <- as.double(Sys.time() - inicio_tempo, units = "secs")

        inicio_tempo <- Sys.time()
        resultado_detec <- detect(modelo_ajustado, dados_serie$value)
        tempo_deteccao <- as.double(Sys.time() - inicio_tempo, units = "secs")

        list(
          md          = modelo_ajustado,
          rs          = resultado_detec,
          dataref     = i,
          modelname   = nome_modelo,
          datasetname = nome_base,
          seriesname  = nome_serie,
          time_fit    = tempo_ajuste,
          time_detect = tempo_deteccao
        )
      }, error = function(e) {
        message(sprintf("Erro em %s - %s: %s", nome_modelo, nome_serie, e$message))
        NULL
      })
    }
    atomic_save(detalhes_modelo, arq_cache, compress = "xz")
  }

  detalhes_todos <- c(detalhes_todos, detalhes_modelo)
}


## ------------------------------------------------------------
## 4) Sumário de desempenho (tempo e métricas) ----
## ------------------------------------------------------------
linhas_resumo <- vector("list", length(detalhes_todos))

count_true <- function(x) {
  # Conta "verdadeiros" em formatos comuns (logical, 0/1, data.frame$event)
  if (is.null(x)) return(NA_integer_)
  if (is.data.frame(x)) {
    if ("event" %in% names(x)) {
      return(sum(as.logical(x$event), na.rm = TRUE))
    } else {
      return(NA_integer_)
    }
  }
  # vetor / matrix / ts
  return(sum(as.logical(x), na.rm = TRUE))
}

for (k in seq_along(detalhes_todos)) {
  exp_k   <- detalhes_todos[[k]]
  dados_k <- series_ts[[exp_k$dataref]]

  # Tamanho da série (linhas)
  tam_serie <- if (!is.null(nrow(dados_k))) nrow(dados_k) else length(dados_k$value)

  # Número de eventos reais (TRUE em dados_k$event)
  num_eventos <- if ("event" %in% names(dados_k)) {
    count_true(dados_k$event)
  } else {
    NA_integer_
  }

  # Número de detecções (TRUE em exp_k$rs$event)
  num_deteccoes <- count_true(exp_k$rs$event)

  # Avaliação "soft" com janela deslizante (ajuste sw_size conforme o caso)
  inicio_tempo <- Sys.time()
  avaliacao_soft <- evaluate(
    har_eval_soft(sw_size = 10),
    exp_k$rs$event,
    if ("event" %in% names(dados_k)) dados_k$event else rep(FALSE, tam_serie)
  )
  tempo_metrica <- as.double(Sys.time() - inicio_tempo, units = "secs")

  get1 <- function(lst, name, default = NA) {
    val <- lst[[name]]
    if (is.null(val) || length(val) == 0) return(default)
    val[[1]]
  }

  # extrai os valores antes do data.frame
  n_cases_simple  <- as.integer(get1(avaliacao_soft, "n_cases_simple",  NA_integer_))
  n_cases_medium  <- as.integer(get1(avaliacao_soft, "n_cases_medium",  NA_integer_))
  n_cases_complex <- as.integer(get1(avaliacao_soft, "n_cases_complex", NA_integer_))
  sigma_max_de3   <- as.numeric(get1(avaliacao_soft, "sigma_max_de3",   NA_real_))

  # calcula o booleano (TRUE/FALSE)
  linear_behavior <- !is.na(sigma_max_de3) &&
    !is.na(num_eventos) &&
    !is.na(num_deteccoes) &&
    sigma_max_de3 < (num_eventos + num_deteccoes)

  # Linha do resumo para esta série e método
  linhas_resumo[[k]] <- data.frame(
    method        = exp_k$modelname,
    dataset       = exp_k$datasetname,
    series        = exp_k$seriesname,
    time_fit      = exp_k$time_fit,
    time_detect   = exp_k$time_detect,
    time_metric   = tempo_metrica,
    precision     = avaliacao_soft$precision,
    recall        = avaliacao_soft$recall,
    f1            = avaliacao_soft$F1,
    tam_serie     = tam_serie,
    num_eventos   = num_eventos,
    num_deteccoes = num_deteccoes,
    n_cases_simple  = n_cases_simple,
    n_cases_medium  = n_cases_medium,
    n_cases_complex = n_cases_complex,
    sigma_max_de3   = sigma_max_de3,
    linear_behavior = linear_behavior,
    stringsAsFactors = FALSE
  )
}

resumo_experimentos <- do.call(rbind, linhas_resumo)

## ------------------------------------------------------------
## 5) Persistência do sumário ----
## ------------------------------------------------------------
save(resumo_experimentos,
     file = file.path("results", "exp_summary.RData"),
     compress = "xz")

# EXTRA
total_tempo_metric <- sum(resumo_experimentos$time_metric, na.rm = TRUE)
print(total_tempo_metric)

