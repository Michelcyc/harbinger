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

soft_scores <- function(detection, event, k){
  # detection e event são arrays booleanos
  D <- which(detection)
  n <- length(D)
  E <- which(event)
  m <- length(E)

  # Transformar um array booleano em segmento

  expand_around_true <- function(detection, K) {
    n <- length(detection)
    true_indices <- which(detection)  # Encontra os índices onde detection == TRUE

    # Cria um vetor lógico expandido
    expanded <- logical(n)

    for (i in true_indices) {
      # Define os limites do intervalo a ser marcado como TRUE
      start <- max(1, i - K)
      end <- min(n, i + K)
      expanded[start:end] <- TRUE
    }

    return(expanded)
  }

  event_aval <- expand_around_true(event, k)

  # ----------------------------------------- #
  S_d <- numeric(n) #o vetor de scores que preciso retornar
  S_d_counter <- 1
  window_size_counter <- 0
  detections_counter <- 0
  events_counter <- 0
  full_window <- length(event_aval)


  for (idx in 1:(full_window - 1)) {
    if (event_aval[idx] && !event_aval[idx+1]) # [TRUE FALSE] Avaliar
    {
      if (window_size_counter<=2*k+1)
    }
    else if (event_aval[idx] && event_aval[idx+1]) # [TRUE TRUE] Appends
    {

    }
    else if (!event_aval[idx] && !event_aval[idx+1]) # [FALSE FALSE] Check detections
    {

    }

    if (event_aval[idx] && !event_aval[idx+1]) {
      interruptor <- 1 #dentro de uma janela avaliativa
      window_size_counter <- window_size_counter+1
      if (detection[idx]) {
        detections_counter <- detections_counter+1
      }
      else {

      }
    }
    else {
      #A avaliação SEMPRE ocorre aqui, porque é a hora que acaba a janela avaliativa
      if ( (interruptor==1) && (window_size_counter==2*k+1) && (detections_counter > 0) ) {
        # Associar a detecção com maior score #
        S_d[S_d_counter] <- 10000
        # ---------------------#
      }
      else if (interruptor==1 && window_size_counter > 2*k+1 && detections_counter > 0){
        # Algoritmo hungaro

        # ---------------- #
      }
      else if(interruptor==1){
        interruptor <- 0
        detections_counter <- 0
        window_size_counter <- 0
      }

      if (detection[idx]) {
        #Nada a fazer porque já tá zerado
      }
      else {
        #Nada a fazer
      }
      interruptor <- 0
      window_size_counter <- 0
      detections_counter <- 0
    }
  }
  window_size_counter
  detections_counter
  # ------------------------------------------ #

  sw_event_index <- 1 # Vou precisar em varrendo os indices de eventos e detecções até o fim
  sw_detection_index <- 1
  #S_d precisa ser um vetor com os scores de cada detecção (vetor S_d)
  # ou seja, eu preciso ir preenchendo S_d com os scores
  for (segment in segments) { # inicio em segment[1] e fim em segment[2]
    if( (segment[2]-segment[1]) <= (2*k+1) ){
      sw_event_index <- sw_event_index+1
      print(sw_event_index)
    }
  }

  for (j in E) { # j vai conter o índice onde ocorreu o evento
    if (j>)
  }

  #mu é a função que calcula os resultados das funções de pertencimento
  mu <- function(j,i,E,D,k) max(min( (D[i]-(E[j]-k))/k, ((E[j]+k)-D[i])/k ), 0)

  #aqui é necessário que crie uma matrix Mu para cada caso conflituoso separadamente
  Mu <- matrix(NA,nrow = n, ncol = m)
  for(j in 1:m) for(i in 1:n) Mu[i,j] <- mu(j,i,E,D,k)

  E_d <- list()
  for(i in 1:n) E_d[[i]] <- which(Mu[i,] == max(Mu[i,]))

  D_e <- list()
  for(j in 1:m) D_e[[j]] <- which(sapply(1:n, function(i) j %in% E_d[[i]] & Mu[i,j] > 0))

  d_e <- c()
  for(j in 1:m) {
    if(length(D_e[[j]])==0) d_e[j] <- NA
    else d_e[j] <- D_e[[j]][which.max(sapply(D_e[[j]], function(i) Mu[i,j]))]
  }

  S_e <- c()
  for(j in 1:m) {
    if(length(D_e[[j]])==0) S_e[j] <- NA
    #else S_e[j] <- sum(sapply(D_e[[j]], function(i) Mu[i,j])) / length(D_e[[j]]) #mean
    else S_e[j] <- max(sapply(D_e[[j]], function(i) Mu[i,j]))  #max
  }

  S_d <- c()
  for(i in 1:n) S_d[i] <- max(S_e[which(d_e == i)], 0)

  return(S_d)
}

#'@importFrom daltoolbox evaluate
#'@export
evaluate.har_eval_soft <- function(obj, detection, event, ...) {
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
