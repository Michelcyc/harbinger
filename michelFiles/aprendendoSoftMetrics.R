load_library("daltoolbox")
load_library("harbinger")
source("my_utils.R")
data("har_examples")

dataset <- har_examples[[15]]

indexDaSerie <- 1:length(dataset$serie)
plot_ts(x=indexDaSerie, y=dataset$serie)

har_model <- hanr_arima()
har_fitted_model <- fit(har_model, dataset$serie)
fitted_detection_model <- detect(obj = har_fitted_model, dataset$serie)
grf <- har_plot(har_fitted_model, dataset$serie, fitted_detection_model, dataset$event)
plot(grf)

hardEval <- evaluate(har_eval(), fitted_detection_model$event, dataset$event)
printEval(hardEval)

softEval <- evaluate(har_eval_soft(), fitted_detection_model$event, dataset$event)
printEval(softEval)

printEvalComparison(softEval, hardEval)
