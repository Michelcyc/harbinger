#'@title Evaluation of event detection
#'@description Evaluation of event detection using SoftED <doi:10.48550/arXiv.2304.00439>
#'@param sw_size tolerance window size
#'@return `har_eval_soft` object
#'@examples
#'library(daltoolbox)
#'
#'#loading the example database
#'data(examples_anomalies)
#'
#'#Using the simple
#'dataset <- examples_anomalies$simple
#'head(dataset)
#'
#'# setting up time change point using GARCH
#'model <- hcp_garch()
#'
#'# fitting the model
#'model <- fit(model, dataset$serie)
#'
#'# making detections
#'detection <- detect(model, dataset$serie)
#'
#'# filtering detected events
#'print(detection[(detection$event),])
#'
#'# evaluating the detections
#'evaluation <- evaluate(har_eval_soft(), detection$event, dataset$event)
#'print(evaluation$confMatrix)
#'
#'# ploting the results
#'grf <- har_plot(model, dataset$serie, detection, dataset$event)
#'plot(grf)
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
  detection_score <- function(d,e,k) max(min( (d-(e-k))/k, ((e+k)-d)/k ), 0)

  complex_cases_association <- function(D_mini, E_mini, k) {
    n <- length(D_mini)
    m <- length(E_mini)

    Mu <- matrix(NA, nrow = n, ncol = m)
    for (j in 1:m) {
      for (i in 1:n) {
        Mu[i, j] <- detection_score(D_mini[i], E_mini[j], k)
      }
    }

    associationMatrix <- RcppHungarian::HungarianSolver(-1 * Mu)
    scores <- Mu[associationMatrix$pairs]
    return(scores)
  }

  soft_scores <- function(detection, event, k){
    #testando modificação
    # detection and event are boolean arrays
    D <- which(detection)
    n <- length(D)
    E <- which(event)
    m <- length(E)

    # Create the initial segments and sort them
    segments <- t(vapply(E, function(x) c(inf = x - k, sup = x + k), numeric(2)))

    # Function to merge overlapping intervals
    merge_intervals <- function(intervals) {
      merged <- list()
      current <- intervals[1, ]
      if (nrow(intervals) > 1) {
        for (i in 2:nrow(intervals)) {
          interval <- intervals[i, ]
          if (interval["inf"] <= current["sup"]) {
            current["sup"] <- interval["sup"]
          } else {
            merged[[length(merged) + 1]] <- current
            current <- interval
          }
        }
      }
      merged[[length(merged) + 1]] <- current
      merged_matrix <- do.call(rbind, merged)
      return(merged_matrix)
    }

    merged_segments <- merge_intervals(segments)

    # Contadores solicitados
    simple_count  <- 0L  # n==1 && m==1
    medium_count  <- 0L  # (n==1 && m>1) ou (n>1 && m==1)
    complex_count <- 0L  # (n>1 && m>1)

    sigma_max_de3 <- 0.0

    # Para cada grupo, computar scores e incrementar contadores
    groups <- lapply(1:nrow(merged_segments), function(i) {
      seg <- merged_segments[i, ]
      D_mini <- D[D >= seg["inf"] & D <= seg["sup"]]
      E_mini <- E[E >= seg["inf"] & E <= seg["sup"]]
      list(D_mini = D_mini, E_mini = E_mini)
    })

    S_d <- rep(0, length(D))
    S_d_counter <- 1L

    for (idx in seq_along(groups)) {
      D_mini <- groups[[idx]]$D_mini
      E_mini <- groups[[idx]]$E_mini

      n <- length(D_mini)
      m <- length(E_mini)

      if (n==1 && m==1) {                       # simples
        simple_count <- simple_count + 1L
        S_d[S_d_counter] <- detection_score(D_mini[1], E_mini[1], k)
        S_d_counter <- S_d_counter + 1L

      } else if (n==1 && m>1) {                 # médio (um D para vários E)
        medium_count <- medium_count + 1L
        valores <- detection_score(D_mini[1], E_mini, k)
        S_d[S_d_counter] <- max(valores)
        S_d_counter <- S_d_counter + 1L

      } else if (n>1 && m==1) {                 # médio (vários D para um E)
        medium_count <- medium_count + 1L
        valores <- detection_score(D_mini, E_mini[1], k)
        S_d[S_d_counter] <- max(valores)
        S_d_counter <- S_d_counter + 1L

      } else if (n > 1 && m > 1) {              # complexo
        complex_count <- complex_count + 1L
        sigma_max_de3 <- sigma_max_de3 + (max(n, m))^3

        scores <- complex_cases_association(D_mini, E_mini, k)
        S_d[S_d_counter:(S_d_counter + length(scores) - 1)] <- scores
        S_d_counter <- S_d_counter + length(scores)
      }
    }

    # Retorna scores + contadores
    return(list(
      scores = S_d,
      n_cases_simple  = simple_count,
      n_cases_medium  = medium_count,
      n_cases_complex = complex_count,
      sigma_max_de3   = sigma_max_de3
    ))
  }

  detection[is.na(detection)] <- FALSE

  # Obs.: mantive o comportamento original de early return quando não há detecções ou eventos
  if ((sum(detection) == 0) || (sum(event) == 0)) {
    return(evaluate(har_eval(), detection, event))
  }

  ss <- soft_scores(detection, event, obj$sw_size)

  scores <- ss$scores
  m <- length(which(event))
  t <- length(event)

  TPs <- sum(scores)
  FPs <- sum(1 - scores)
  FNs <- m - TPs
  TNs <- (t - m) - FPs

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
  NPV <- (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + (specificity*(1-prevalence)))
  detection_rate <- TPs/(TPs+FPs+FNs+TNs)
  detection_prevalence <- (TPs+FPs)/(TPs+FPs+FNs+TNs)
  balanced_accuracy <- (sensitivity+specificity)/2
  precision <- TPs/(TPs+FPs)
  recall <- TPs/(TPs+FNs)

  beta <- 1
  F1 <- (1+beta^2)*precision*recall/((beta^2 * precision)+recall)

  Ps <- TPs+FPs/(TPs+FPs+FNs+TNs)
  Ns <- FNs+TNs/(TPs+FPs+FNs+TNs)
  Ts <- TPs+TNs/(TPs+FPs+FNs+TNs)
  Fs <- FPs+FNs/(TPs+FPs+FNs+TNs)

  s_metrics <- list(
    TPs=TPs, FPs=FPs, FNs=FNs, TNs=TNs,
    confMatrix=confMatrix,
    accuracy=accuracy, sensitivity=sensitivity, specificity=specificity,
    prevalence=prevalence, PPV=PPV, NPV=NPV,
    detection_rate=detection_rate, detection_prevalence=detection_prevalence,
    balanced_accuracy=balanced_accuracy, precision=precision, recall=recall, F1=F1,
    Ps=Ps, Ns=Ns, Ts=Ts, Fs=Fs,
    # >>> NOVOS CAMPOS:
    n_cases_simple  = ss$n_cases_simple,
    n_cases_medium  = ss$n_cases_medium,
    n_cases_complex = ss$n_cases_complex,
    sigma_max_de3   = ss$sigma_max_de3
  )

  return(s_metrics)
}
