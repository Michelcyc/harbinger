mu <- function(j,i,E,D,k) max(min( (D[i]-(E[j]-k))/(2*k), ((E[j]+k)-D[i])/(2*k) ), 0)
mu_simples <- function(d,e,k) max(min( (d-(e-k))/(2*k), ((e+k)-d)/(2*k) ), 0)

m <- length(which(event))
t <- length(event)
m_adjusted <- m/obj$sw_size
t_adjusted <- t/obj$sw_size
total_adjusted <- 1/obj$sw_size

TPs <- sum(scores)
FPs <- (sum(1 - scores)) #o score to TP naturalmente regula o FP se o K aumentar
FNs <- (m_adjusted-TPs)
TNs <- t_adjusted-FPs

#TPs <- sum(scores)
#FPs <- sum(1-scores)
#FNs <- m-TPs
#TNs <- (t-m)-FPs
