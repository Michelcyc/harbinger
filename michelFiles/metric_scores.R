#### #### #### #### #### #### #### #### ####
#### BOXPLOT PRECISION RECALL E F1 ##########
#### #### #### #### #### #### #### ####

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# Mantém só as métricas e empilha em formato longo
df_long <- resumo_experimentos %>%
  select(precision, recall, f1) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  filter(is.finite(value))  # remove NA/NaN/Inf

# Boxplot
ggplot(df_long, aes(x = metric, y = value, fill = metric)) +
  geom_boxplot(outlier.alpha = 0.25) +
  labs(x = NULL, y = "Score", title = "Distribuição de métricas") +
  theme_minimal() +
  theme(legend.position = "none")

###########################################
## Salvando métricas ####
###########################################

#nome <- "metricas_SoftED.RData"
#nome <- "metricas_SoftEDPAR.RData"
nome <- "metricas_smartSoftED.RData"
metricas_exp <- resumo_experimentos %>%
  select(precision, recall, f1)

save(metricas_exp, file = nome)

###########################################
## 3 BOX PLOTS DOS 3 (9) ####
###########################################

library(dplyr)
library(tidyr)
library(ggplot2)

# carrega cada experimento
load("metricas_SoftED.RData")  # cria metricas_exp1
metricas_exp1 <- metricas_exp %>% mutate(experimento = "exp1")

load("metricas_SoftEDPAR.RData")
metricas_exp2 <- metricas_exp %>% mutate(experimento = "exp2")

load("metricas_smartSoftED.RData")
metricas_exp3 <- metricas_exp %>% mutate(experimento = "exp3")

###########################################
## Coordenadas paralelas ####
###########################################

# Precisão
df_prec <- tibble::tibble(
  softed      = metricas_exp1$precision,
  softedpar   = metricas_exp2$precision,
  smartsofted = metricas_exp3$precision
)

ggparcoord(
  data        = df_prec,
  columns     = 1:3,
  scale       = "globalminmax",
  showPoints  = FALSE,
  alphaLines  = 0.1
) +
  labs(
    title = "Coordenadas paralelas (Precisão)",
    x = NULL,
    y = "Precisão"
  ) +
  theme_minimal()

# Revocação
df_rec <- tibble::tibble(
  softed      = metricas_exp1$recall,
  softedpar   = metricas_exp2$recall,
  smartsofted = metricas_exp3$recall
)

ggparcoord(
  data        = df_rec,
  columns     = 1:3,
  scale       = "globalminmax",
  showPoints  = FALSE,
  alphaLines  = 0.1
) +
  labs(
    title = "Coordenadas paralelas (Revocação)",
    x = NULL,
    y = "Revocação"
  ) +
  theme_minimal()

