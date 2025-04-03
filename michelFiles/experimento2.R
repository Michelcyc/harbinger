remove.packages("harbinger")
quit(save = "no")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")

install.packages('RcppHungarian')
library(RcppHungarian)

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

set.seed(7)

n_methods <- 7

# Criar um for loop para cada. Ajustar 2 loops: loop de datasets e loop de detectores
#  ------------------------------ HARD test --------------------------- #


for (i in 1:length(datasets)) {
  for (j in 1:n_methods) {
    #Tests
    dataset <- datasets[[i]]
    if (j==1) { # ARIMA
      model <- hanr_arima()
      model <- fit(model, dataset$serie)
      detection <- detect(obj = model, dataset$serie)
    }
    else if (j==2) { # FBIAD
      model <- hanr_fbiad()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==3) { #DTW
      model <- hanct_dtw(3)
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==4) { # K-means
      model <- hanct_kmeans(3)
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==5) { # FFT
      model <- hanr_fft()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==6) { # GARCH
      model <- hanr_garch()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==7) { #Wavelet
      model <- hanr_wavelet()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    # Metrics
    execution_time <- system.time({
      eval <- evaluate(har_eval(), detection$event, dataset$Classe)
    })
    index <- ( (i-1)* n_methods + j)
    print(paste(index, " from i=", i, "and j=", j))
    for (name in names(eval)) {
      hard[[name]][[index]] <- eval[[name]]
    }
    hard[['time']][[index]] <- unname(execution_time['elapsed'])
  }
}

save(hard, file = "michelFiles/hard.RData")

#  ------------------------------ END HARD test --------------------------- #

#  ------------------------------ SOFT1 test --------------------------- #
for (i in 1:length(datasets)) {
  for (j in 1:n_methods) {
    #Tests
    dataset <- datasets[[i]]
    if (j==1) { # ARIMA
      model <- hanr_arima()
      model <- fit(model, dataset$serie)
      detection <- detect(obj = model, dataset$serie)
    }
    else if (j==2) { # FBIAD
      model <- hanr_fbiad()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==3) { #DTW
      model <- hanct_dtw(3)
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==4) { # K-means
      model <- hanct_kmeans(3)
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==5) { # FFT
      model <- hanr_fft()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==6) { # GARCH
      model <- hanr_garch()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==7) { #Wavelet
      model <- hanr_wavelet()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    # Metrics
    execution_time <- system.time({
      eval <- evaluate(har_eval_soft(sw_size=3), detection$event, dataset$Classe)   # SW = 3
    })
    index <- ( (i-1)* n_methods + j)
    print(paste(index, " from i=", i, "and j=", j))
    for (name in names(eval)) {
      soft1[[name]][[index]] <- eval[[name]]
    }
    soft1[['time']][[index]] <- unname(execution_time['elapsed'])
  }
}

save(soft1, file = "michelFiles/soft1.RData")

#  ------------------------------ END SOFT1 test --------------------------- #

#  ------------------------------ SOFT2 test --------------------------- #
for (i in 1:length(datasets)) {
  for (j in 1:n_methods) {
    #Tests
    dataset <- datasets[[i]]
    if (j==1) { # ARIMA
      model <- hanr_arima()
      model <- fit(model, dataset$serie)
      detection <- detect(obj = model, dataset$serie)
    }
    else if (j==2) { # FBIAD
      model <- hanr_fbiad()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==3) { #DTW
      model <- hanct_dtw(3)
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==4) { # K-means
      model <- hanct_kmeans(3)
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==5) { # FFT
      model <- hanr_fft()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==6) { # GARCH
      model <- hanr_garch()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    else if (j==7) { #Wavelet
      model <- hanr_wavelet()
      model <- fit(model, dataset$serie)
      detection <- detect(model, dataset$serie)
    }
    # Metrics
    execution_time <- system.time({
      eval <- evaluate(har_eval_soft(sw_size=3), detection$event, dataset$Classe)   # SW = 3
    })
    index <- ( (i-1)* n_methods + j)
    print(paste(index, " from i=", i, "and j=", j))
    for (name in names(eval)) {
      soft2[[name]][[index]] <- eval[[name]]
    }
    soft2[['time']][[index]] <- unname(execution_time['elapsed'])
  }
}

save(soft2, file = "michelFiles/soft2.RData")

mean(unlist(soft2$time))
mean(unlist(soft2$accuracy))
mean(unlist(soft2$sensitivity))
mean(unlist(soft2$specificity))
mean(unlist(soft2$prevalence))

