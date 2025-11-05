## ============================================================
## 0) (re)instala e carrega pacotes
## ============================================================
remove.packages("harbinger")
quit(save = "no")
devtools::install_github("Michelcyc/harbinger@minha-trilha-nova", force = TRUE, upgrade = "never")

suppressPackageStartupMessages({
  library(daltoolbox)
  library(daltoolboxdp)
  library(tspredit)
  library(harbinger)
  library(united)
})

## ============================================================
## 1) Funções utilitárias
## ============================================================

safe_get <- function(lst, i) {
  if (is.null(lst)) return(NULL)
  if (is.character(i)) {
    if (length(lst) == 0L) return(NULL)
    if (is.null(names(lst))) return(NULL)
    return(lst[[i]])
  }
  if (i > 0 && i <= length(lst)) lst[[i]] else NULL
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
## 2) Métodos
## ============================================================
w <- 30L
input_size <- 5
prep <- tspredit::ts_norm_gminmax()

metodos <- list(
  #remd   = hanr_remd(noise = 0.05, trials = 2),        # REMD
  fbiad  = hanr_fbiad(sw_size = w),                   # FBIAD (w=30)
  arima  = hanr_arima(),                               # ARIMA (defaults)

  # Regressão por ML (mesma janela w e preprocess)
  lstm   = hanr_ml(
    daltoolboxdp::ts_lstm(preprocess = prep, input_size = input_size, epochs = 10000L),
    sw_size = w
  ),
  elm    = hanr_ml(
    tspredit::ts_elm(preprocess = prep, input_size = input_size, actfun = "purelin"),
    sw_size = w
  )
#  conv1d = hanr_ml(
#    daltoolboxdp::ts_conv1d(preprocess = prep, input_size = input_size, epochs = 10000L),
#    sw_size = w
#  ),
#  svm    = hanr_ml(
#    tspredit::ts_svm(preprocess = prep, input_size = input_size, kernel = "radial"),
#    sw_size = w
#  )
)

names(metodos)
## ============================================================
## 3) Datasets
## ============================================================
# univariados (do código original)
dataset_names_uni <- c(
  "ucr_ecg", "ucr_int_bleeding", "ucr_nasa", "ucr_power_demand",
  "mit_bih_MLII", "mit_bih_V1", "mit_bih_V2", "mit_bih_V5",
  "A1Benchmark", "A2Benchmark", "A3Benchmark", "A4Benchmark",
  "nab_artificialWithAnomaly", "nab_realAWSCloudwatch",
  "nab_realAdExchange", "nab_realKnownCause",
  "nab_realTraffic", "nab_realTweets",
  "gecco"
)

# multivariados
dataset_names_mv <- c(
  "oil_3w_Type_1",
  "oil_3w_Type_2",
  "oil_3w_Type_4",
  "oil_3w_Type_5",
  "oil_3w_Type_6",
  "oil_3w_Type_7",
  "oil_3w_Type_8"
)

## ============================================================
## 4) Estruturas gerais
## ============================================================
dir.create("results", showWarnings = FALSE, recursive = TRUE)

detalhes_todos <- list()  # vai receber univariado + multivariado
series_map     <- list()  # para lookup na avaliação

## ============================================================
## 5) 1ª parte: DATASETS UNIVARIADOS
## ============================================================
for (dataset_name in dataset_names_uni) {
  message("------------------------------------------------------------")
  message("Processando (univariado): ", dataset_name)

  ok <- try({
    data(list = dataset_name)
  }, silent = TRUE)

  if (inherits(ok, "try-error") || !exists(dataset_name, inherits = TRUE)) {
    message("   >> Aviso: dataset '", dataset_name, "' não encontrado. Pulando.")
    next
  }

  data_obj <- get(dataset_name, inherits = TRUE)

  # normaliza para lista de data.frames
  if (is.data.frame(data_obj)) {
    series_ts <- list(data_obj)
    names(series_ts) <- dataset_name
  } else if (is.list(data_obj)) {
    series_ts <- data_obj
    if (is.null(names(series_ts))) {
      names(series_ts) <- paste0(dataset_name, "_", seq_along(series_ts))
    }
  } else {
    message("   >> Aviso: dataset '", dataset_name, "' não é data.frame nem lista. Pulando.")
    next
  }

  for (i in seq_along(series_ts)) {
    if (is.null(nrow(series_ts[[i]]))) {
      stop(sprintf("Objeto %s[%d] não é um data.frame esperado.", dataset_name, i))
    }
  }

  # guarda para lookup
  series_map[[dataset_name]] <- series_ts

  # roda métodos
  for (j in seq_along(metodos)) {
    modelo_atual    <- metodos[[j]]
    nome_modelo     <- names(metodos)[j]
    detalhes_modelo <- list()

    arq_cache <- file.path("results",
                           sprintf("exp_detail_%s_%s.RData", dataset_name, nome_modelo))

    if (file.exists(arq_cache)) {
      load(arq_cache)
      if (is.list(detalhes_modelo)) {
        detalhes_todos <- c(detalhes_todos, detalhes_modelo)
        message("   >> (uni) usando cache de ", dataset_name, " / ", nome_modelo,
                " (", length(detalhes_modelo), " séries).")
        next
      } else {
        detalhes_modelo <- list()
      }
    }

    message("   >> (uni) Computando ", dataset_name, " / ", nome_modelo, " ...")
    for (i in seq_along(series_ts)) {
      dados_serie <- series_ts[[i]]
      nome_serie  <- names(series_ts)[i]

      result <- safe_get(detalhes_modelo, i)
      if (is.null(result)) {
        detalhes_modelo[[i]] <- tryCatch({
          inicio_tempo <- Sys.time()
          md <- fit(modelo_atual, dados_serie$value)
          tempo_ajuste <- as.double(Sys.time() - inicio_tempo, units = "secs")

          inicio_tempo <- Sys.time()
          rs <- detect(md, dados_serie$value)
          tempo_deteccao <- as.double(Sys.time() - inicio_tempo, units = "secs")

          list(
            md          = md,
            rs          = rs,
            dataref     = i,
            modelname   = nome_modelo,
            datasetname = dataset_name,
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
}

## ============================================================
## 6) 2ª parte: DATASETS MULTIVARIADOS
## ============================================================
for (dataset_name in dataset_names_mv) {
  message("------------------------------------------------------------")
  message("Processando (multivariado): ", dataset_name)

  ok <- try({
    data(list = dataset_name)
  }, silent = TRUE)

  if (inherits(ok, "try-error") || !exists(dataset_name, inherits = TRUE)) {
    message("   >> Aviso: dataset multivariado '", dataset_name, "' não encontrado. Pulando.")
    next
  }

  data_mv <- get(dataset_name, inherits = TRUE)
  nome_base <- dataset_name

  if (!is.list(data_mv) || is.data.frame(data_mv)) {
    message("   >> Aviso: dataset multivariado '", dataset_name, "' não está no formato esperado (list de data.frames). Pulando.")
    next
  }

  # descobre, para cada série, quais colunas são variáveis numéricas
  colunas_variaveis_por_serie <- lapply(data_mv, function(df) {
    if (!is.data.frame(df)) return(character(0))
    drop <- c("event", "idx", "type", "time", "timestamp", "date", "datetime")
    vars <- setdiff(names(df), drop)
    vars[vapply(df[vars], is.numeric, logical(1))]
  })

  # guarda no series_map como "dataset" -> list de séries multivariadas
  # (na hora do resumo vamos pegar por dataref e depois pela variável)
  series_map[[dataset_name]] <- data_mv

  for (j in seq_along(metodos)) {
    modelo_atual    <- metodos[[j]]
    nome_modelo     <- names(metodos)[j]
    detalhes_modelo <- list()

    arq_cache <- file.path("results",
                           sprintf("exp_detail_%s_%s_mv.RData", nome_base, nome_modelo))

    if (file.exists(arq_cache)) {
      load(arq_cache)
      if (!is.list(detalhes_modelo)) {
        detalhes_modelo <- list()
      }
    }

    # percorre (série, variável)
    for (i in seq_along(data_mv)) {
      dados_multi <- data_mv[[i]]
      nome_serie  <- names(data_mv)[i]
      vars_i      <- colunas_variaveis_por_serie[[i]]

      if (length(vars_i) == 0L) {
        message(sprintf("   >> (mv) série '%s' sem variáveis numéricas utilizáveis.", nome_serie))
        next
      }

      for (var in vars_i) {
        key <- sprintf("%s::%s", nome_serie, var)

        if (!is.null(safe_get(detalhes_modelo, key))) {
          next
        }

        detalhes_modelo[[key]] <- tryCatch({
          x <- as.numeric(dados_multi[[var]])
          if (anyNA(x)) x <- replace(x, is.na(x), 0)

          y_event <- if ("event" %in% names(dados_multi)) {
            as.logical(dados_multi$event)
          } else {
            rep(FALSE, length(x))
          }

          t0 <- Sys.time()
          md <- fit(modelo_atual, x)
          time_fit <- as.double(Sys.time() - t0, units = "secs")

          t0 <- Sys.time()
          rs <- detect(md, x)
          time_detect <- as.double(Sys.time() - t0, units = "secs")

          list(
            md          = md,
            rs          = rs,
            dataref     = i,
            varname     = var,
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

        atomic_save(detalhes_modelo, arq_cache, compress = "xz")
      }
    }

    detalhes_todos <- c(detalhes_todos, detalhes_modelo)
  }
}

## ============================================================
## 7) SUMÁRIO ÚNICO
## ============================================================
linhas_resumo <- vector("list", length(detalhes_todos))

for (k in seq_along(detalhes_todos)) {
  exp_k <- detalhes_todos[[k]]
  if (is.null(exp_k)) next

  ds_name <- exp_k$datasetname
  dataref <- exp_k$dataref

  if (!ds_name %in% names(series_map)) {
    warning("Dataset não encontrado em series_map: ", ds_name, " (pulando linha ", k, ")")
    next
  }

  dados_k <- series_map[[ds_name]][[dataref]]

  tam_serie <- if (!is.null(nrow(dados_k))) nrow(dados_k) else length(dados_k$value)

  num_eventos <- if ("event" %in% names(dados_k)) {
    count_true(dados_k$event)
  } else {
    NA_integer_
  }

  num_deteccoes <- count_true(exp_k$rs$event)

  t0 <- Sys.time()
  avaliacao_soft <- evaluate(
    har_eval_soft(sw_size = 30),
    exp_k$rs$event,
    if ("event" %in% names(dados_k)) dados_k$event else rep(FALSE, tam_serie)
  )
  tempo_metrica <- as.double(Sys.time() - t0, units = "secs")

  n_cases_simple   <- as.integer(get1(avaliacao_soft, "n_cases_simple",  NA_integer_))
  n_cases_medium   <- as.integer(get1(avaliacao_soft, "n_cases_medium",  NA_integer_))
  n_cases_complex  <- as.integer(get1(avaliacao_soft, "n_cases_complex", NA_integer_))
  ed_simple        <- as.integer(get1(avaliacao_soft, "ed_simple",  NA_integer_))
  ed_medium        <- as.integer(get1(avaliacao_soft, "ed_medium",  NA_integer_))
  ed_complex       <- as.integer(get1(avaliacao_soft, "ed_complex", NA_integer_))
  sigma_max_de3    <- as.numeric(get1(avaliacao_soft, "sigma_max_de3",   NA_real_))

  linear_behavior <- !is.na(sigma_max_de3) &&
    !is.na(num_eventos) &&
    !is.na(num_deteccoes) &&
    sigma_max_de3 < (num_eventos + num_deteccoes)

  linhas_resumo[[k]] <- data.frame(
    method          = exp_k$modelname,
    dataset         = exp_k$datasetname,
    series          = exp_k$seriesname,
    variable        = if (!is.null(exp_k$varname)) exp_k$varname else NA_character_,
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
    ed_simple       = ed_simple,
    ed_medium       = ed_medium,
    ed_complex      = ed_complex,
    sigma_max_de3   = sigma_max_de3,
    linear_behavior = linear_behavior,
    stringsAsFactors = FALSE
  )
}

resumo_experimentos <- do.call(rbind, linhas_resumo)

## ============================================================
## 8) SALVA ÚNICO
## ============================================================
save(resumo_experimentos,
     file = file.path("results", "exp_summary.RData"),
     compress = "xz")

total_tempo_metric <- sum(resumo_experimentos$time_metric, na.rm = TRUE)
print(total_tempo_metric)
