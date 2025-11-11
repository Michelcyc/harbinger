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

# depois de gerar resumo_experimentos
#nome <- "metricas_SoftED.RData"
nome <- "metricas_SoftEDPAR.RData"
#nome <- "metricas_smartSoftED.RData"
metricas_exp <- resumo_experimentos %>%
  select(precision, recall, f1)

save(metricas_exp, file = nome)

# Box plots das 3 #

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

# empilha tudo
df_all <- bind_rows(metricas_exp1, metricas_exp2, metricas_exp3)

# formato longo
df_long_all <- df_all %>%
  pivot_longer(c(precision, recall, f1),
               names_to = "metric",
               values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  filter(is.finite(value))
ggplot(df_long_all, aes(x = metric, y = value, fill = experimento)) +
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.alpha = 0.25) +
  labs(x = NULL, y = "Score", title = "Distribuição das métricas por experimento") +
  theme_minimal()
#Se você quiser só 3 boxplots (um experimento só), é basicamente o código que você já tem,
# usando df_long em vez de df_long_all.


## Gráfico de coordenadas paralelas ##

# metricas_exp1 , 2, e 3 já obtidos da etapa anterior

# Aqui pressuponho que as linhas estão na mesma ordem nos 3 objetos
df_coord <- tibble::tibble(
  Exp1_Precision = metricas_exp1$precision,
  Exp2_Precision = metricas_exp2$precision,
  Exp3_Precision = metricas_exp3$precision,
  Exp1_Recall    = metricas_exp1$recall,
  Exp2_Recall    = metricas_exp2$recall,
  Exp3_Recall    = metricas_exp3$recall
)

# Plot dos precisions

install.packages("GGally")  # se ainda não tiver
library(GGally)
library(ggplot2)

ggparcoord(
  data        = df_coord,
  columns     = 1:3,            # as 6 colunas na ordem desejada
  scale       = "globalminmax", # tudo na escala [0,1]
  showPoints  = FALSE,
  alphaLines  = 0.3
) +
  labs(
    title = "Coordenadas paralelas: Precision (E1–E3) e Recall (E1–E3)",
    x = NULL,
    y = "Score (normalizado)"
  ) +
  theme_minimal()

# Plot dos recalls

install.packages("GGally")  # se ainda não tiver
library(GGally)
library(ggplot2)

ggparcoord(
  data        = df_coord,
  columns     = 4:6,            # as 6 colunas na ordem desejada
  scale       = "globalminmax", # tudo na escala [0,1]
  showPoints  = FALSE,
  alphaLines  = 0.3
) +
  labs(
    title = "Coordenadas paralelas: Precision (E1–E3) e Recall (E1–E3)",
    x = NULL,
    y = "Score (normalizado)"
  ) +
  theme_minimal()

