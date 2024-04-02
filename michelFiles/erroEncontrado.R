load_library("daltoolbox")
load_library("harbinger")
library("daltoolbox")
library("harbinger")

source("my_utils.R")
data("har_examples")


dataset <- har_examples[[4]]  #erro no F1
#dataset <- har_examples[[11]]  #erro em vários

indexDaSerie <- 1:length(dataset$serie)
plot_ts(x=indexDaSerie, y=dataset$serie)

har_model <- hanr_arima()
har_fitted_model <- fit(har_model, dataset$serie)
fitted_detection_model <- detect(obj = har_fitted_model, dataset$serie)
grf <- har_plot(har_fitted_model, dataset$serie, fitted_detection_model, dataset$event)
plot(grf)

hardEval <- evaluate(har_eval(), fitted_detection_model$event, dataset$event)
printEval(hardEval)  # muitos NaN

#erro na softEvaluation
softEval <- evaluate(har_eval_soft(), fitted_detection_model$event, dataset$event)
printEval(softEval)

printEvalComparison(softEval, hardEval)

#podem dar erro de divisao por zero
#sensitivity <- TPs/(TPs+FNs)
#specificity <- TNs/(FPs+TNs)
#precision <- TPs/(TPs+FPs)
#recall <- TPs/(TPs+FNs)
#F1 <- (1+beta^2)*precision*recall/((beta^2 * precision)+recall)
#e outras medidas que a análise é um pouco mais complexa