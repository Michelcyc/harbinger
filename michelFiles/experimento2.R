remove.packages("harbinger")
quit(save = "no")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")

install.packages('RcppHungarian')
library(RcppHungarian)

library("daltoolbox")
library("harbinger")
library(readr)
source("michelFiles/my_utils.R")

dataset <- read_csv('michelFiles/meus_dados.csv')

#indexDaSerie <- 1:length(dataset$PreprocessedSeries)
#plot_ts(x=indexDaSerie, y=dataset$PreprocessedSeries)

har_model <- hanr_arima()
har_fitted_model <- fit(har_model, dataset$PreprocessedSeries)
fitted_detection_model <- detect(obj = har_fitted_model, dataset$PreprocessedSeries)
#grf <- har_plot(har_fitted_model, dataset$PreprocessedSeries, fitted_detection_model, dataset$Classe)
#plot(grf)

newSoftEval <- evaluate(har_eval_soft(sw_size=10), fitted_detection_model$event, dataset$Classe)
printEval(newSoftEval)

hardEval <- evaluate(har_eval(), fitted_detection_model$event, dataset$Classe)
printEval(hardEval)

execution_time <- system.time({
  hardEval <- evaluate(har_eval(), fitted_detection_model$event, dataset$Classe)
})
