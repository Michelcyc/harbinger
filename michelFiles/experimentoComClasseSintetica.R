remove.packages("harbinger")
quit(save = "no")

devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")

install.packages('RcppHungarian')
library(RcppHungarian)

library("daltoolbox")
library("harbinger")
source("michelFiles/my_utils.R")



data("har_examples")

dataset <- har_examples[[2]]
dataset[30,1] <- 0.1
dataset[30,2] <- TRUE
dataset[51,1] <- 0.8
dataset[51,2] <- TRUE
dataset[54,1] <- 0.2
dataset[54,2] <- TRUE
dataset[90,2] <- TRUE

indexDaSerie <- 1:length(dataset$serie)
plot_ts(x=indexDaSerie, y=dataset$serie)

har_model <- hanr_arima()
har_fitted_model <- fit(har_model, dataset$serie)
fitted_detection_model <- detect(obj = har_fitted_model, dataset$serie)
grf <- har_plot(har_fitted_model, dataset$serie, fitted_detection_model, dataset$event)
plot(grf)
# Passando os dados
event <- dataset$event
detection <- fitted_detection_model$event
k=2
# ----

newSoftEval <- evaluate(har_eval_soft(sw_size=10), fitted_detection_model$event, dataset$event)
printEval(newSoftEval)

hardEval <- evaluate(har_eval(), fitted_detection_model$event, dataset$event)
printEval(hardEval)

save(softEval, file = "softEval_data.Rdata")
