# Pacotes necessários
library(daltoolbox)
library(daltoolboxdp)
library(tspredit)
library(harbinger)
library(united)

safe_get <- function(lst, i) {
  if (i > 0 && i <= length(lst)) {
    lst[[i]]
  } else {
    NULL
  }
}

## ------------------------------------------------------------
## 1) Preparação dos métodos (modelos) ----
## ------------------------------------------------------------
metodos <- list(
  hanr_arima(),   # arima
  han_autoencoder(3, 2, autoenc_ed, num_epochs = 1500),  # autoencoder

  hanr_ml(ts_mlp(ts_norm_gminmax(), input_size=3, size=3, decay=0)), # mlp
  hanr_ml(ts_elm(ts_norm_gminmax(), input_size=4, nhid=3, actfun="purelin")), # elm
  hanr_ml(ts_rf(ts_norm_gminmax(), input_size=4, nodesize=1, ntree=20)), # rf
  hanr_ml(ts_svm(ts_norm_gminmax(), input_size=4,  kernel = "radial")), # svm
  hanr_ml(ts_lstm(ts_norm_gminmax(), input_size=4, epochs=2000)), # lstm
  hanr_ml(ts_conv1d(ts_norm_gminmax(), input_size=4, epochs=2000)), # conv1d

  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_mlp(ts_norm_gminmax()),
                  ranges = list(size = 1:10, decay = seq(0, 1, 1/9), maxit=2000)), sw_size = 30), # mlp_hyper

  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_elm(ts_norm_gminmax()),
                  ranges = list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas', 'relu', 'purelin'))), sw_size = 30), # elm_hyper

  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_rf(ts_norm_gminmax()),
                  ranges = list(nodesize=1:10, ntree=1:10)), sw_size = 30), # rf_hyper

  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_svm(ts_norm_gminmax()),
                  ranges = list(kernel=c("sigmoid"), epsilon=seq(0, 1, 0.1), cost=seq(20, 100, 20))), sw_size = 30), # svm_hyper

  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_lstm(ts_norm_gminmax()),
                  ranges = list(epochs = c(2000))), sw_size = 30), # lstm_hyper

  hanr_ml(ts_tune(input_size=c(3:7), base_model = ts_conv1d(ts_norm_gminmax()),
                  ranges = list(epochs = c(2000))), sw_size = 30) # convid_hyper

)
names(metodos) <- c("arima", "autoencoder", "mlp", "elm", "rf", "svm", "lstm", "conv1d",
                    "mlp_hyper", "elm_hyper", "rf_hyper", "svm_hyper", "lstm_hyper", "conv1d_hyper")

## ------------------------------------------------------------
## 2) Preparação dos dados ----
## ------------------------------------------------------------
nome_base <- "gecco"
data(gecco)  # carrega a base 'gecco' no ambiente

# Fatiamos cada série no mesmo intervalo [16500:18000]
# OBS: ajuste este recorte se o tamanho das séries variar.
series_ts <- vector("list", length(gecco) - 1)

# em um primeiro momento só um para testarmos as otimizações de hiperparametros
series_ts <- vector("list", 1)

for (i in seq_along(series_ts)) {
  serie_nome <- names(gecco)[i]
  # Verificação de limites para evitar erro se a série for menor
  n <- nrow(gecco[[i]])
  inicio <- 16500L
  fim    <- 18000L
  if (is.null(n)) {
    stop(sprintf("Objeto %s não é um data.frame/ts esperado.", serie_nome))
  }
  if (fim > n) {
    stop(sprintf("Série %s tem apenas %d linhas; ajuste o recorte (%d:%d).",
                 serie_nome, n, inicio, fim))
  }
  series_ts[[i]] <- gecco[[i]][inicio:fim, ]
  names(series_ts)[i] <- serie_nome
}

## Garante diretório de resultados
dir.create("results", showWarnings = FALSE, recursive = TRUE)

## ------------------------------------------------------------
## 3) Detecção detalhada (com cache por método) ----
## ------------------------------------------------------------
detalhes_todos <- list()

for (j in seq_along(metodos)) {                 # percorre métodos
  modelo_atual   <- metodos[[j]]
  nome_modelo    <- names(metodos)[j]
  detalhes_modelo <- list()                     # resultados por série deste método

  # Caminho do arquivo de cache para este método
  arq_cache <- file.path("results", sprintf("exp_detail_%s.RData", nome_modelo))

  # Se existir resultado pré-computado, carregar para continuar de onde parou
  if (file.exists(arq_cache)) {
    load(file = arq_cache)  # carrega objeto 'detalhes_modelo' se salvo anteriormente
  }

  for (i in seq_along(series_ts)) {             # percorre séries
    dados_serie <- series_ts[[i]]
    nome_serie  <- names(series_ts)[i]

    result <- safe_get(detalhes_modelo, i)

    if (is.null(result)) {

      # Se ainda não existe resultado para esta série, processa
      detalhes_modelo[[i]] <- tryCatch({
        ## 3.1 Ajuste (fit)
        inicio_tempo <- Sys.time()
        modelo_ajustado <- fit(modelo_atual, dados_serie$value)
        tempo_ajuste <- as.double(Sys.time() - inicio_tempo, units = "secs")

        ## 3.2 Detecção (detect)
        inicio_tempo <- Sys.time()
        resultado_detec <- detect(modelo_ajustado, dados_serie$value)
        tempo_deteccao <- as.double(Sys.time() - inicio_tempo, units = "secs")

        ## 3.3 Empacota resultado desta série
        result <- list(
          md          = modelo_ajustado,
          rs          = resultado_detec,
          dataref     = i,                 # índice da série
          modelname   = nome_modelo,
          datasetname = nome_base,
          seriesname  = nome_serie,
          time_fit    = tempo_ajuste,
          time_detect = tempo_deteccao
        )
        names(result)[i] <- sprintf("%s_%s", nome_base, nome_serie)

        ## se tudo deu certo, devolve NULL (ou qualquer valor de sucesso que preferir)
        result
      }, error = function(e) {
        message(sprintf("Erro em %s - %s: %s", nome_modelo, nome_serie, e$message))
        ## devolve o índice que falhou
        NULL
      })
    }
    ## 3.4 Salva cache incremental
    save(detalhes_modelo, file = arq_cache, compress = "xz")
  }

  ## Acumula os detalhes deste método no agregado geral
  detalhes_todos <- c(detalhes_todos, detalhes_modelo)
}

## ------------------------------------------------------------
## 4) Sumário de desempenho (tempo e métricas) ----
## ------------------------------------------------------------
linhas_resumo <- vector("list", length(detalhes_todos))
for (k in seq_along(detalhes_todos)) {
  exp_k   <- detalhes_todos[[k]]
  dados_k <- series_ts[[exp_k$dataref]]

  # Avaliação "soft" com janela deslizante (ajuste sw_size conforme o caso)
  avaliacao_soft <- evaluate(har_eval_soft(sw_size = 10),
                             exp_k$rs$event, dados_k$event)

  # Linha do resumo para esta série e método
  linhas_resumo[[k]] <- data.frame(
    method      = exp_k$modelname,
    dataset     = exp_k$datasetname,
    series      = exp_k$seriesname,
    time_fit    = exp_k$time_fit,
    time_detect = exp_k$time_detect,
    precision   = avaliacao_soft$precision,
    recall      = avaliacao_soft$recall,
    f1          = avaliacao_soft$F1,
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
