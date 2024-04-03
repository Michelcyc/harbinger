library("daltoolbox")
remove.packages("harbinger")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")
devtools::load_all("/home/michel/R/x86_64-pc-linux-gnu-library/4.3/harbinger")
library("harbinger")
packageVersion("harbinger")
source("michelFiles/my_utils.R")
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

