# Algumas notas sobre o funcionamento de har_eval_soft

har_eval_soft <- function(sw_size = 15) {
  obj <- har_eval()
  obj$sw_size <- sw_size
  class(obj) <- append("har_eval_soft", class(obj))
  return(obj)
} # Retorna um objeto do tipo har_eval_soft

soft_scores <- function(detection, event, k){
  E <- which(event) # vetor com índices onde o evento ocorreu
  m <- length(E) # tamanho desse vetor
  
  D <- which(detection) # vetor com índices onde o algoritmo detectou um evento
  n <- length(D) # tamanho desse vetor
  
  mu <- function(j,i,E,D,k) max(min( (D[i]-(E[j]-k))/k, ((E[j]+k)-D[i])/k ), 0)
  #uma funcao de pertencimento/pertinencia
  
  Mu <- matrix(NA,nrow = n, ncol = m)
  for(j in 1:m) for(i in 1:n) Mu[i,j] <- mu(j,i,E,D,k)
  #uma matriz com os resultados das relações de pertinencia entre cada evento e deteccao
  
  E_d <- list()
  for(i in 1:n) E_d[[i]] <- which(Mu[i,] == max(Mu[i,]))
  #uma lista que correlaciona o index da detecção ao
  # 1) o index de um evento associado
  # ou 2) uma lista dos index de eventos associados (em caso de empate)
  # PS: Quando nenhum evento for associado à uma detecção todos os eventos
  # entrarão na lista de eventos associados
  
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