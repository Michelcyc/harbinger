remove.packages("harbinger")
quit(save = "no")

install.packages('RcppHungarian')
library(RcppHungarian)

devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")

library("daltoolbox")
library("harbinger")
library(readr)


# HARD #
hard <- list(
  TP = vector("list", 70),
  FP = vector("list", 70),
  FN = vector("list", 70),
  TN = vector("list", 70),
  accuracy = vector("list", 70),
  sensitivity = vector("list", 70),
  specificity = vector("list", 70),
  prevalence = vector("list", 70),
  PPV = vector("list", 70),
  NPV = vector("list", 70),
  detection_rate = vector("list", 70),
  detection_prevalence = vector("list", 70),
  balanced_accuracy = vector("list", 70),
  precision = vector("list", 70),
  recall = vector("list", 70),
  F1 = vector("list", 70),
  time = vector("list", 70)
)

# SOFT_1 #
soft1 <- list(
  TP = vector("list", 70),
  FP = vector("list", 70),
  FN = vector("list", 70),
  TN = vector("list", 70),
  accuracy = vector("list", 70),
  sensitivity = vector("list", 70),
  specificity = vector("list", 70),
  prevalence = vector("list", 70),
  PPV = vector("list", 70),
  NPV = vector("list", 70),
  detection_rate = vector("list", 70),
  detection_prevalence = vector("list", 70),
  balanced_accuracy = vector("list", 70),
  precision = vector("list", 70),
  recall = vector("list", 70),
  F1 = vector("list", 70),
  time = vector("list", 70)
)

# SOFT_2 #
soft2 <- list(
  TP = vector("list", 70),
  FP = vector("list", 70),
  FN = vector("list", 70),
  TN = vector("list", 70),
  accuracy = vector("list", 70),
  sensitivity = vector("list", 70),
  specificity = vector("list", 70),
  prevalence = vector("list", 70),
  PPV = vector("list", 70),
  NPV = vector("list", 70),
  detection_rate = vector("list", 70),
  detection_prevalence = vector("list", 70),
  balanced_accuracy = vector("list", 70),
  precision = vector("list", 70),
  recall = vector("list", 70),
  F1 = vector("list", 70),
  time = vector("list", 70)
)

# SOFT_3 #
soft3 <- list(
  TP = vector("list", 70),
  FP = vector("list", 70),
  FN = vector("list", 70),
  TN = vector("list", 70),
  accuracy = vector("list", 70),
  sensitivity = vector("list", 70),
  specificity = vector("list", 70),
  prevalence = vector("list", 70),
  PPV = vector("list", 70),
  NPV = vector("list", 70),
  detection_rate = vector("list", 70),
  detection_prevalence = vector("list", 70),
  balanced_accuracy = vector("list", 70),
  precision = vector("list", 70),
  recall = vector("list", 70),
  F1 = vector("list", 70),
  time = vector("list", 70)
)

set.seed(7)

n_methods <- 7

# Testes #
detections <- vector("list", length(datasets) * n_methods)

for (i in 1:length(datasets)) {
  for (j in 1:n_methods) {
    dataset <- datasets[[i]]
    print(paste("Rodando dataset", i, "com método", j))
    # Escolha do modelo
    if (j == 1) {
      model <- hanr_arima()
    } else if (j == 2) {
      model <- hanr_fbiad()
    } else if (j == 3) {
      model <- hanr_fft_amoc() #antigo hanct_dtw(3)
    } else if (j == 4) {
      model <- hanct_kmeans()
    } else if (j == 5) {
      model <- hanr_fft()
    } else if (j == 6) {
      model <- hanr_garch()
    } else if (j == 7) {
      model <- hanr_wavelet()
    }

    model <- tryCatch({
      model$har_outliers <- hutils$har_outliers_boxplot
      fit(model, dataset$serie)
    }, error = function(e) {
      message(paste("Erro no fit() para i =", i, "j =", j, ":", e$message))
      NULL
    })

    if (!is.null(model)) {
      detection <- tryCatch({
        detect(model, dataset$serie)
      }, error = function(e) {
        message(paste("Erro no detect() para i =", i, "j =", j, ":", e$message))
        NULL
      })

      if (!is.null(detection)) {
        index <- (i - 1) * n_methods + j
        detections[[index]] <- detection
      }
    }
  }
}

save(detections, file = "michelFiles/detections.RData")
############ Getting metrics #############

loaded_name <- load("michelFiles/detections.RData")
detections <- get(loaded_name)

# Avaliar HARD
for (i in 1:length(datasets)) {
  for (j in 1:n_methods) {
    dataset <- datasets[[i]]
    index <- (i - 1) * n_methods + j
    detection <- detections[[index]]

    execution_time <- system.time({
      eval <- evaluate(har_eval(), detection$event, dataset$Classe)
    })

    for (name in names(eval)) {
      hard[[name]][[index]] <- eval[[name]]
    }
    hard[['time']][[index]] <- unname(execution_time['elapsed'])
  }
}
save(hard, file = "michelFiles/hard.RData")


#  ------------------------------ END HARD test --------------------------- #

avaliar_soft <- function(soft_list, detections, datasets, n_methods, k) {
  for (i in 1:length(datasets)) {
    for (j in 1:n_methods) {
      index <- (i - 1) * n_methods + j
      detection <- detections[[index]]
      dataset <- datasets[[i]]

      execution_time <- system.time({
        eval <- evaluate(har_eval_soft(k), detection$event, dataset$Classe)
      })

      for (name in names(eval)) {
        soft_list[[name]][[index]] <- eval[[name]]
      }
      soft_list[['time']][[index]] <- unname(execution_time['elapsed'])
    }
  }
  return(soft_list)
}

# Depois chame assim:
soft1 <- avaliar_soft(soft1, detections, datasets, n_methods, 3)
save(soft1, file = "michelFiles/soft1.RData")
soft2 <- avaliar_soft(soft2, detections, datasets, n_methods, 3)
save(soft2, file = "michelFiles/soft2.RData")
soft3 <- avaliar_soft(soft3, detections, datasets, n_methods, 3)
save(soft3, file = "michelFiles/soft3.RData")

mean(unlist(soft2$time))
mean(unlist(soft2$accuracy))
mean(unlist(soft2$sensitivity))
mean(unlist(soft2$specificity))
mean(unlist(soft2$prevalence))

