#' @title Evaluation of event detection (SoftED)
#' @description Soft evaluation of event detection using SoftED <doi:10.48550/arXiv.2304.00439>.
#' @param sw_size Integer. Tolerance window size for soft matching.
#' @return `har_eval_soft` object
#'
#' @examples
#' library(daltoolbox)
#'
#' # Load anomaly example data
#' data(examples_anomalies)
#'
#' # Use the simple series
#' dataset <- examples_anomalies$simple
#' head(dataset)
#'
#' # Configure a change-point detector (GARCH)
#' model <- hcp_garch()
#'
#' # Fit the detector
#' model <- fit(model, dataset$serie)
#'
#' # Run detection
#' detection <- detect(model, dataset$serie)
#'
#' # Show detected events
#' print(detection[(detection$event),])
#'
#' # Evaluate detections (SoftED)
#' evaluation <- evaluate(har_eval_soft(), detection$event, dataset$event)
#' print(evaluation$confMatrix)
#'
#' # Plot the results
#' grf <- har_plot(model, dataset$serie, detection, dataset$event)
#' plot(grf)
#'
#' @references
#' - Salles, R., Lima, J., Reis, M., Coutinho, R., Pacitti, E., Masseglia, F., Akbarinia, R.,
#'   Chen, C., Garibaldi, J., Porto, F., Ogasawara, E. SoftED: Metrics for soft evaluation of
#'   time series event detection. Computers and Industrial Engineering, 2024.
#'   doi:10.1016/j.cie.2024.110728
#'@export
har_eval_soft <- function(sw_size = 15) {
  obj <- har_eval()
  obj$sw_size <- sw_size
  class(obj) <- append("har_eval_soft", class(obj))
  return(obj)
}


#'@importFrom daltoolbox evaluate
#'@importFrom RcppHungarian HungarianSolver
#'@exportS3Method evaluate har_eval_soft
evaluate.har_eval_soft <- function(obj, detection, event, ...) {
  soft_scores <- function(detection, event, k){
    E <- which(event)
    m <- length(E)

    D <- which(detection)
    n <- length(D)

    mu <- function(j,i,E,D,k) max(min( (D[i]-(E[j]-k))/k, ((E[j]+k)-D[i])/k ), 0)

    Mu <- matrix(NA,nrow = n, ncol = m)
    for(j in 1:m) for(i in 1:n) Mu[i,j] <- mu(j,i,E,D,k)

    associationMatrix <- HungarianSolver(-1 * Mu)
    pairs <- associationMatrix$pairs

    # Normaliza formatos possíveis em uma matrix de 2 colunas: (row, col)
    if (is.null(pairs) || length(pairs) == 0) {
      scores <- numeric(0)
    } else {
      if (is.data.frame(pairs)) pairs <- as.matrix(pairs)
      if (is.vector(pairs) && !is.matrix(pairs) && length(pairs) %% 2 == 0) {
        pairs <- matrix(pairs, ncol = 2, byrow = TRUE)
      }
      if (!is.matrix(pairs) || ncol(pairs) != 2) {
        stop("Formato inesperado em associationMatrix$pairs (esperado matrix 2-col).")
      }

      row_idx <- as.integer(pairs[,1])
      col_idx <- as.integer(pairs[,2])

      # filtra pares fora dos limites por segurança
      valid <- which(row_idx >= 1 & row_idx <= nrow(Mu) & col_idx >= 1 & col_idx <= ncol(Mu))
      if (length(valid) == 0) {
        scores <- numeric(0)
      } else {
        scores <- Mu[cbind(row_idx[valid], col_idx[valid])]
      }
    }
    return(scores)
  }

  detection[is.na(detection)] <- FALSE

  if((sum(detection)==0) || (sum(event)==0)){
    return(evaluate(har_eval(), detection, event))
  }

  scores <- soft_scores(detection, event, obj$sw_size)

  m <- length(which(event))
  t <- length(event)

  TPs <- sum(scores)
  FPs <- sum(1-scores)
  FNs <- m-TPs
  TNs <- (t-m)-FPs

  confMatrix <- as.table(matrix(c(as.character(TRUE),as.character(FALSE),
                                  round(TPs,2),round(FPs,2),
                                  round(FNs,2),round(TNs,2)), nrow = 3, ncol = 2, byrow = TRUE,
                                dimnames = list(c("detection", "TRUE","FALSE"),
                                                c("event", ""))))

  accuracy <- (TPs+TNs)/(TPs+FPs+FNs+TNs)
  sensitivity <- TPs/(TPs+FNs)
  specificity <- TNs/(FPs+TNs)
  prevalence <- (TPs+FNs)/(TPs+FPs+FNs+TNs)
  PPV <- (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
  NPV <- (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))
  detection_rate <- TPs/(TPs+FPs+FNs+TNs)
  detection_prevalence <- (TPs+FPs)/(TPs+FPs+FNs+TNs)
  balanced_accuracy <- (sensitivity+specificity)/2
  precision <- TPs/(TPs+FPs)
  recall <- TPs/(TPs+FNs)

  beta <- 1
  F1 <- (1+beta^2)*precision*recall/((beta^2 * precision)+recall)

  s_metrics <- list(TPs=TPs,FPs=FPs,FNs=FNs,TNs=TNs,confMatrix=confMatrix,accuracy=accuracy,
                    sensitivity=sensitivity, specificity=specificity,
                    prevalence=prevalence, PPV=PPV, NPV=NPV,
                    detection_rate=detection_rate, detection_prevalence=detection_prevalence,
                    balanced_accuracy=balanced_accuracy, precision=precision,
                    recall=recall, F1=F1)

  return(s_metrics)
}
