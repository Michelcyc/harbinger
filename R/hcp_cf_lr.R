#'@description Ancestor class for time series event detection
#'@details The Harbinger class establishes the basic interface for time series event detection.
#'  Each method should be implemented in a descendant class of Harbinger
#'@return Harbinger object
#'@examples detector <- harbinger()
#'@export
#'@import forecast
#'@import rugarch
#'@import TSPred
hcp_cf_lr <- function(w = 30, alpha = 1.5) {
  obj <- harbinger()
  obj$alpha <- alpha
  obj$w <- w
  class(obj) <- append("hcp_cf_lr", class(obj))
  return(obj)
}

#'@export
detect.hcp_cf_lr <- function(obj, serie) {
  linreg <- function(serie) {
    data <- data.frame(t = 1:length(serie), x = serie)
    return(lm(x~t, data))
  }

  n <- length(serie)
  non_na <- which(!is.na(serie))

  serie <- na.omit(serie)

  #Adjusting a model to the entire series
  M1 <- linreg(serie)

  #Adjustment error on the entire series
  s <- residuals(M1)^2
  outliers <- har_outliers_idx(s, obj$alpha)
  group_outliers <- split(outliers, cumsum(c(1, diff(outliers) != 1)))
  outliers <- rep(FALSE, length(s))
  for (g in group_outliers) {
    if (length(g) > 0) {
      i <- min(g)
      outliers[i] <- TRUE
    }
  }
  outliers[1:obj$w] <- FALSE

  y <- TSPred::mas(s, obj$w)

  #Adjusting to the entire series
  M2 <- linreg(y)

  #Adjustment error on the whole window
  u <- residuals(M2)^2

  u <- TSPred::mas(u, obj$w)
  cp <- har_outliers_idx(u)
  group_cp <- split(cp, cumsum(c(1, diff(cp) != 1)))
  cp <- rep(FALSE, length(u))
  for (g in group_cp) {
    if (length(g) > 0) {
      i <- min(g)
      cp[i] <- TRUE
    }
  }
  cp[1:obj$w] <- FALSE
  cp <- c(rep(FALSE, length(s)-length(u)), cp)

  i_outliers <- rep(NA, n)
  i_outliers[non_na] <- outliers

  i_cp <- rep(NA, n)
  i_cp[non_na] <- cp

  detection <- data.frame(idx=1:n, event = i_outliers, type="")
  detection$type[i_outliers] <- "anomaly"
  detection$event[cp] <- TRUE
  detection$type[cp] <- "hcp_scp"

  return(detection)
}
