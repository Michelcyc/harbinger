remove.packages("harbinger")
quit(save = "no")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")

install.packages('RcppHungarian')
library(RcppHungarian)

library("daltoolbox")
library("harbinger")
library(readr)
source("michelFiles/my_utils.R")

dataset <- read_csv('michelFiles/futuros_ibov_2019.csv')
names(dataset)[names(dataset) == "PreprocessedSeries"] <- "serie"
dataset <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_aus200_2019.csv')
dataset <- head(dataset, 20000)

#indexDaSerie <- 1:length(dataset$PreprocessedSeries)
#plot_ts(x=indexDaSerie, y=dataset$PreprocessedSeries)

# ARIMA
model <- hanr_arima()
model <- fit(model, dataset$serie)
detection <- detect(obj = model, dataset$serie)
#grf <- har_plot(har_fitted_model, dataset$PreprocessedSeries, fitted_detection_model, dataset$Classe)
#plot(grf)

model <- hanr_remd()
model <- fit(model, dataset$serie)
detection <- detect(model, dataset$serie)

execution_time <- system.time({
  newSoftEval <- evaluate(har_eval_soft(sw_size=2), detection$event, dataset$Classe)
  printEval(newSoftEval)
})
execution_time


execution_time <- system.time({
  hardEval <- evaluate(har_eval(), detection$event, dataset$Classe)
  printEval(hardEval)
})
execution_time


# ------- Teste massivo com 2 ---------- #

# HARD #
hard <- list(
  TP = vector("list", 100),
  FP = vector("list", 100),
  FN = vector("list", 100),
  TN = vector("list", 100),
  accuracy = vector("list", 100),
  sensitivity = vector("list", 100),
  specificity = vector("list", 100),
  prevalence = vector("list", 100),
  PPV = vector("list", 100),
  NPV = vector("list", 100),
  detection_rate = vector("list", 100),
  detection_prevalence = vector("list", 100),
  balanced_accuracy = vector("list", 100),
  precision = vector("list", 100),
  recall = vector("list", 100),
  F1 = vector("list", 100),
  time = vector("list", 100)
)

# SOFT_1 #
soft1 <- list(
  TP = vector("list", 100),
  FP = vector("list", 100),
  FN = vector("list", 100),
  TN = vector("list", 100),
  accuracy = vector("list", 100),
  sensitivity = vector("list", 100),
  specificity = vector("list", 100),
  prevalence = vector("list", 100),
  PPV = vector("list", 100),
  NPV = vector("list", 100),
  detection_rate = vector("list", 100),
  detection_prevalence = vector("list", 100),
  balanced_accuracy = vector("list", 100),
  precision = vector("list", 100),
  recall = vector("list", 100),
  F1 = vector("list", 100),
  time = vector("list", 100)
)

# SOFT_2 #
soft2 <- list(
  TP = vector("list", 100),
  FP = vector("list", 100),
  FN = vector("list", 100),
  TN = vector("list", 100),
  accuracy = vector("list", 100),
  sensitivity = vector("list", 100),
  specificity = vector("list", 100),
  prevalence = vector("list", 100),
  PPV = vector("list", 100),
  NPV = vector("list", 100),
  detection_rate = vector("list", 100),
  detection_prevalence = vector("list", 100),
  balanced_accuracy = vector("list", 100),
  precision = vector("list", 100),
  recall = vector("list", 100),
  F1 = vector("list", 100),
  time = vector("list", 100)
)

# Criar um for loop para cada. Ajustar 2 loops: loop de datasets e loop de detectores

#  ------------------------------ HARD test --------------------------- #
n_methods <- 8

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
    else if (j==8) { # EMD
      model <- hanr_emd()
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
  for (j in 1:2) {
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



    # Metrics
    execution_time <- system.time({
      eval <- evaluate(har_eval_soft(sw_size=3), detection$event, dataset$Classe)   # SW = 3
    })
    index <- ( (i-1)* 2 + j)  # AJUSTAR o 2 para 10 !!!!!!!!
    print(index)
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
for (i in 1:2) {
  #Tests


  # Metrics
  execution_time <- system.time({
    eval <- evaluate(har_eval_soft(sw_size=3), detection$event, dataset$Classe)
  })
  for (name in names(eval)) {
    soft2[[name]][[i]] <- eval[[name]]
  }
  soft2[['time']][[i]] <- unname(execution_time['elapsed'])
}

# soft2 ARMAZENAR em .Rdata

#  ------------------------------ END SOFT2 test --------------------------- #
