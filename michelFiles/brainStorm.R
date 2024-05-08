#Install package
install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/event_datasets", force=TRUE)
library(dalevents)

#Load a series
data(eia_oil_prices)
series <- eia_oil_prices$

#Use the loaded series
summary(series)
plot(series$series, type = "l")
#or
plot(series$value, type = "l")

#Get information about a series
?yahoo_a1
