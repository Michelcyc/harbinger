remove.packages("harbinger")
quit(save = "no")
library("daltoolbox")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")
library("harbinger")
source("michelFiles/my_utils.R")

install.packages('RcppHungarian')
library(RcppHungarian)

data(examples_anomalies)

# Using the simple time series
dataset <- examples_anomalies$multiple


# install.packages(c("ggplot2","patchwork")) # se precisar
library(ggplot2)
library(patchwork)

df <- transform(dataset, t = seq_len(nrow(dataset)))

p1 <- ggplot(df, aes(t, serie)) +
  geom_line() +
  labs(y = "série", x = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_blank())

p2 <- ggplot(df, aes(t, as.integer(event))) +
  geom_col(width = 1) +
  scale_y_continuous(breaks = c(0,1), labels = c("FALSE","TRUE"), limits = c(0,1)) +
  labs(y = "evento", x = "t") +
  theme_minimal(base_size = 11)

(p1 / p2) + plot_layout(heights = c(3,1))




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
