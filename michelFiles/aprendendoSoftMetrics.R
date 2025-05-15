remove.packages("harbinger")
quit(save = "no")
library("daltoolbox")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")
library("harbinger")
source("michelFiles/my_utils.R")

install.packages('RcppHungarian')
library(RcppHungarian)

data("har_examples")

dataset <- har_examples[[2]]

indexDaSerie <- 1:length(dataset$serie)
plot_ts(x=indexDaSerie, y=dataset$serie)

har_model <- hanr_arima()
har_fitted_model <- fit(har_model, dataset$serie)
fitted_detection_model <- detect(obj = har_fitted_model, dataset$serie)
grf <- har_plot(har_fitted_model, dataset$serie, fitted_detection_model, dataset$event)
plot(grf)

softEval <- evaluate(har_eval_soft(sw_size=10), fitted_detection_model$event, dataset$event)
printEval(softEval)

hardEval <- evaluate(har_eval(), fitted_detection_model$event, dataset$event)
printEval(hardEval)

save(softEval, file = "softEval_data.Rdata")
