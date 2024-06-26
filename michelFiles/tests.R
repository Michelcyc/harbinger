#ela tem uma funcao hard_metrics que chama uma funcao chamada metrics
# o hard_evaluate recebe as series e um vetor que seleciona uma metric e usa retorna um par chave-valor com a metrica escolhida

#Se o teste da Rebecca retornou só uma metrica escolhida e o har_eval atual retorna todas as métricas
# eu preciso pegar esse retorno e selecionar dentro dele a métrica que me importa

# Load the .RData file
load("datasets_evts_ref.RData")

#Carregando pacote
remove.packages("harbinger")
quit(save = "no")
library("daltoolbox")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")
library("harbinger")
source("michelFiles/my_utils.R")

install.packages('RcppHungarian')
library(RcppHungarian)

data("har_examples")

dataset <- har_examples[[2]]

indexDaSerie <- 1:length(dataset$serie)
plot_ts(x=indexDaSerie, y=dataset$serie)


