#ela tem uma funcao hard_metrics que chama uma funcao chamada metrics
# o hard_evaluate recebe as series e um vetor que seleciona uma metric e usa retorna um par chave-valor com a metrica escolhida

#Se o teste da Rebecca retornou só uma metrica escolhida e o har_eval atual retorna todas as métricas
# eu preciso pegar esse retorno e selecionar dentro dele a métrica que me importa

#Carregando pacote
remove.packages("harbinger")
quit(save = "no")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")
library("harbinger")
install.packages('RcppHungarian')
library(RcppHungarian)

# Load the .RData file
load("softed-main/experiment_code/datasets_evts_ref.RData")
# Load datasets
devtools::install_github("cefet-rj-dal/event_datasets", force=TRUE)
library(dalevents)

#Profundidades
item <- evts_yahoo[[1]][[1]][[1]]
item <- evts_NAB[[1]][[1]][[1]]
item <- evts_tmn[[1]][[1]][[1]]
item <- evts_3w[[1]][[1]][[1]][[1]]


#source("michelFiles/funcoesQuePrecisoUsar.R")

data(numenta_artificialNoAnomaly) # nenhum evento
data(numenta_artificialWithAnomaly) # nenhum evento conflitante
data(numenta_realAdExchange) # nenhum evento conflitante
data(numenta_realAWSCloudwatch) # nenhum evento conflitante
data(numenta_realKnownCause) # nenhum evento conflitante
data(numenta_realTweets) # nenhum evento conflitante
data(numenta_realTraffic)
#Série temporal número 4 tem apenas uma situação de evento conflitante com janela de 60

data(oil_3w_Type_1)
data(oil_3w_Type_2)
data(oil_3w_Type_5)
data(oil_3w_Type_6)
data(oil_3w_Type_7)
data(oil_3w_Type_8)
# Todas as séries do 3W tem bastantes eventos conflitantes

# Verificar se existem eventos K proximos no Numenta
k=60
eventos_proximos = 0
for (i in numenta_realTraffic) {
  for (j in i) {
    event_vector <- j$event
    indices_of_ones <- which(event_vector == 1)
    if (length(indices_of_ones) > 0) {
      differences <- diff(indices_of_ones)
      nearby_ones <- differences < k
      eventos_proximos <- sum(nearby_ones)
    }
    print(eventos_proximos)
  }
}

# Verificar se existem eventos K proximos no 3W
k=60
eventos_proximos = 0
for (i in oil_3w_Type_8) {
  for (j in i) {
    event_vector <- j$class
    indices_of_ones <- which(event_vector == 1)
    if (length(indices_of_ones) > 0) {
      differences <- diff(indices_of_ones)
      nearby_ones <- differences < k
      eventos_proximos <- sum(nearby_ones)
    }
    print(eventos_proximos)
  }
}

event_vector <- oil_3w_Type_1[[1]][[1]]
number_of_ones <- sum(event_vector == 1)
print(number_of_ones)

event_vector <- numenta_artificialWithAnomaly$artificialWithAnomaly$art_daily_jumpsdo$event
number_of_ones <- sum(event_vector == 1)
print(number_of_ones)

event_vector <- numenta_artificialWithAnomaly$artificialWithAnomaly$art_daily_jumps$event
number_of_ones <- sum(event_vector == 1)
print(number_of_ones)

event_vector <- numenta_artificialWithAnomaly$artificialWithAnomaly$art_daily_noju$event
number_of_ones <- sum(event_vector == 1)
print(number_of_ones)

event_vector <- numenta_artificialWithAnomaly$artificialWithAnomaly$art_increase_spike_densi$event
number_of_ones <- sum(event_vector == 1)
print(number_of_ones)
