## ============================================================
## Setup e instalações (se necessário)
## ============================================================

remove.packages("harbinger")
quit(save = "no")

install.packages('RcppHungarian')
library(RcppHungarian)

# devtools::install_github("Michelcyc/harbinger", force = TRUE, upgrade = "never")
devtools::install_github("Michelcyc/harbinger@minha-trilha-nova", force = TRUE, upgrade = "never")

# Carrega (de novo) após instalar
library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(harbinger)
library(united)

## ============================================================
## Funções utilitárias
## ============================================================

safe_get <- function(lst, i) {
  if (i > 0 && i <= length(lst)) lst[[i]] else NULL
}

# Save atômico para cache
atomic_save <- function(x, path, compress = "xz") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  nm  <- deparse(substitute(x))                         # salva com o mesmo nome do objeto
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  save(list = nm, file = tmp, compress = compress)      # escreve no tmp primeiro
  ok <- file.rename(tmp, path)                          # move atômico no mesmo FS
  if (!ok) {
    ok2 <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
    if (!ok2) stop("Falha ao gravar cache em: ", path)
  }
}

count_true <- function(x) {
  if (is.null(x)) return(NA_integer_)
  if (is.data.frame(x)) {
    if ("event" %in% names(x)) {
      return(sum(as.logical(x$event), na.rm = TRUE))
    } else {
      return(NA_integer_)
    }
  }
  return(sum(as.logical(x), na.rm = TRUE))
}

get1 <- function(lst, name, default = NA) {
  val <- lst[[name]]
  if (is.null(val) || length(val) == 0) return(default)
  val[[1]]
}

## ============================================================
## 1) Preparação dos métodos (modelos)
## ============================================================

metodos <- list(
  hanr_fbiad(),  # Método 1: FBIAD
  hanr_arima()   # Método 2: ARIMA
)
names(metodos) <- c("fbiad", "arima")

## ============================================================
## 2) Lista de datasets a processar
## ============================================================
# OBS: Se algum nome não existir na sua versão das bases, ele será ignorado com aviso.
dataset_names <- c(
  "ucr_ecg", "ucr_int_bleeding", "ucr_nasa", "ucr_power_demand",
  "mit_bih_MLII", "mit_bih_V1", "mit_bih_V2", "mit_bih_V5",
  "A1Benchmark", "A2Benchmark", "A3Benchmark", "A4Benchmark",
  "nab_artificialWithAnomaly", "nab_realAWSCloudwatch",
  "nab_realAdExchange", "nab_realKnownCause",
  "nab_realTraffic", "nab_realTweets",
  "gecco"
)

## ============================================================
## 3) Detecção detalhada (com cache por dataset + método)
##     - Acumula em 'detalhes_todos'
##     - Mantém 'series_map' para lookup na Parte 4
## ============================================================

dir.create("results", showWarnings = FALSE, recursive = TRUE)

detalhes_todos <- list()
series_map     <- list()  # mapeia dataset_name -> lista de séries (data.frames)

for (dataset_name in dataset_names) {

  message("------------------------------------------------------------")
  message("Processando dataset: ", dataset_name)

  # tenta carregar o dataset
  ok <- try({
    data(list = dataset_name)  # carrega no ambiente
  }, silent = TRUE)

  if (inherits(ok, "try-error") || !exists(dataset_name, inherits = TRUE)) {
    message("   >> Aviso: dataset '", dataset_name, "' não encontrado. Pulando.")
    next
  }

  data_obj <- get(dataset_name, inherits = TRUE)

  # Normaliza para lista de data.frames (cada série)
  if (is.data.frame(data_obj)) {
    series_ts <- list(data_obj)
    names(series_ts) <- dataset_name
  } else if (is.list(data_obj)) {
    series_ts <- data_obj
    if (is.null(names(series_ts))) {
      # se vier sem nomes, cria nomes artificiais
      names(series_ts) <- paste0(dataset_name, "_", seq_along(series_ts))
    }
  } else {
    message("   >> Aviso: dataset '", dataset_name, "' não é data.frame nem lista. Pulando.")
    next
  }

  # sanity check das séries
  for (i in seq_along(series_ts)) {
    if (is.null(nrow(series_ts[[i]]))) {
      stop(sprintf("Objeto %s[%d] não é um data.frame esperado.", dataset_name, i))
    }
  }

  # salva no mapa para uso na Parte 4
  series_map[[dataset_name]] <- series_ts

  # Para cada método, processa e usa cache por (dataset, método)
  for (j in seq_along(metodos)) {
    modelo_atual    <- metodos[[j]]
    nome_modelo     <- names(metodos)[j]
    detalhes_modelo <- list()

    # cache inclui dataset + método
    arq_cache <- file.path("results", sprintf("exp_detail_%s_%s.RData", dataset_name, nome_modelo))

    if (file.exists(arq_cache)) {
      # carrega 'detalhes_modelo' do cache e acumula
      load(file = arq_cache)  # carrega objeto detalhes_modelo
      if (!is.list(detalhes_modelo)) {
        message("   >> Cache inválido para ", dataset_name, " / ", nome_modelo, " — ignorando cache.")
        detalhes_modelo <- list()
      } else {
        # acumula no total e segue para o próximo método (sem recomputar)
        detalhes_todos <- c(detalhes_todos, detalhes_modelo)
        message("   >> Usando cache de ", dataset_name, " / ", nome_modelo, " (", length(detalhes_modelo), " séries).")
        next
      }
    }

    # Não havia cache válido — computa
    message("   >> Computando ", dataset_name, " / ", nome_modelo, " ...")
    for (i in seq_along(series_ts)) {
      dados_serie <- series_ts[[i]]
      nome_serie  <- names(series_ts)[i]

      result <- safe_get(detalhes_modelo, i)
      if (is.null(result)) {
        detalhes_modelo[[i]] <- tryCatch({
          # Ajuste
          inicio_tempo <- Sys.time()
          modelo_ajustado <- fit(modelo_atual, dados_serie$value)
          tempo_ajuste <- as.double(Sys.time() - inicio_tempo, units = "secs")

          # Detecção
          inicio_tempo <- Sys.time()
          resultado_detec <- detect(modelo_ajustado, dados_serie$value)
          tempo_deteccao <- as.double(Sys.time() - inicio_tempo, units = "secs")

          list(
            md          = modelo_ajustado,
            rs          = resultado_detec,
            dataref     = i,                  # índice dentro do dataset corrente
            modelname   = nome_modelo,
            datasetname = dataset_name,       # <-- importante: identifica de qual dataset veio
            seriesname  = nome_serie,
            time_fit    = tempo_ajuste,
            time_detect = tempo_deteccao
          )
        }, error = function(e) {
          message(sprintf("Erro em %s - %s: %s", nome_modelo, nome_serie, e$message))
          NULL
        })
      }

      # salva cache incremental (atômico)
      atomic_save(detalhes_modelo, arq_cache, compress = "xz")
    }

    # acumula no agregado geral
    detalhes_todos <- c(detalhes_todos, detalhes_modelo)
  }
}

## ============================================================
## 4) Sumário de desempenho (tempo e métricas)
##     (mesmo código que você já tinha, mas usando series_map)
## ============================================================

linhas_resumo <- vector("list", length(detalhes_todos))

for (k in seq_along(detalhes_todos)) {
  exp_k <- detalhes_todos[[k]]

  # resgata a série correta usando o mapa dataset -> lista de séries
  ds_name  <- exp_k$datasetname
  dataref  <- exp_k$dataref
  if (!ds_name %in% names(series_map)) {
    warning("Dataset não encontrado em series_map: ", ds_name, " (pulando linha ", k, ")")
    next
  }
  dados_k <- series_map[[ds_name]][[dataref]]

  # Tamanho da série
  tam_serie <- if (!is.null(nrow(dados_k))) nrow(dados_k) else length(dados_k$value)

  # Número de eventos reais
  num_eventos <- if ("event" %in% names(dados_k)) {
    count_true(dados_k$event)
  } else {
    NA_integer_
  }

  # Número de detecções
  num_deteccoes <- count_true(exp_k$rs$event)

  # Avaliação "soft"
  inicio_tempo <- Sys.time()
  avaliacao_soft <- evaluate(
    har_eval_soft(sw_size = 30),
    exp_k$rs$event,
    if ("event" %in% names(dados_k)) dados_k$event else rep(FALSE, tam_serie)
  )
  tempo_metrica <- as.double(Sys.time() - inicio_tempo, units = "secs")

  # extrai contadores opcionais, se existirem
  n_cases_simple   <- as.integer(get1(avaliacao_soft, "n_cases_simple",  NA_integer_))
  n_cases_medium   <- as.integer(get1(avaliacao_soft, "n_cases_medium",  NA_integer_))
  n_cases_complex  <- as.integer(get1(avaliacao_soft, "n_cases_complex", NA_integer_))
  ed_simple   <- as.integer(get1(avaliacao_soft, "ed_simple",  NA_integer_))
  ed_medium   <- as.integer(get1(avaliacao_soft, "ed_medium",  NA_integer_))
  ed_complex  <- as.integer(get1(avaliacao_soft, "ed_complex", NA_integer_))
  sigma_max_de3    <- as.numeric(get1(avaliacao_soft, "sigma_max_de3",   NA_real_))

  linear_behavior <- !is.na(sigma_max_de3) &&
    !is.na(num_eventos) &&
    !is.na(num_deteccoes) &&
    sigma_max_de3 < (num_eventos + num_deteccoes)

  linhas_resumo[[k]] <- data.frame(
    method          = exp_k$modelname,
    dataset         = exp_k$datasetname,
    series          = exp_k$seriesname,
    time_fit        = exp_k$time_fit,
    time_detect     = exp_k$time_detect,
    time_metric     = tempo_metrica,
    precision       = avaliacao_soft$precision,
    recall          = avaliacao_soft$recall,
    f1              = avaliacao_soft$F1,
    tam_serie       = tam_serie,
    num_eventos     = num_eventos,
    num_deteccoes   = num_deteccoes,
    n_cases_simple  = n_cases_simple,
    n_cases_medium  = n_cases_medium,
    n_cases_complex = n_cases_complex,
    ed_simple        = ed_simple,
    ed_medium        = ed_medium,
    ed_complex       = ed_complex,
    sigma_max_de3   = sigma_max_de3,
    linear_behavior = linear_behavior,
    stringsAsFactors = FALSE
  )
}

resumo_experimentos <- do.call(rbind, linhas_resumo)

## ============================================================
## 5) Persistência do sumário
## ============================================================
save(resumo_experimentos,
     file = file.path("results", "exp_summary.RData"),
     compress = "xz")

# EXTRA
total_tempo_metric <- sum(resumo_experimentos$time_metric, na.rm = TRUE)
print(total_tempo_metric)
