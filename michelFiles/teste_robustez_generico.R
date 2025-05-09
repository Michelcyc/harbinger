remove.packages("harbinger")
quit(save = "no")

install.packages('RcppHungarian')
library(RcppHungarian)

devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")

library("daltoolbox")
library("harbinger")

# Gerar todos os vetores possíveis de 7 valores lógicos (TRUE/FALSE)
dataset1 <- expand.grid(replicate(7, c(TRUE, FALSE), simplify = FALSE))
dataset2 <- expand.grid(replicate(7, c(TRUE, FALSE), simplify = FALSE))

# Inicializar matriz para armazenar F1-scores
results1 <- matrix(NA, nrow = nrow(dataset1), ncol = nrow(dataset2))
results2 <- matrix(NA, nrow = nrow(dataset1), ncol = nrow(dataset2))

# Loop de avaliação: comparar cada linha de dataset1 com cada linha de dataset2
for (i in seq_len(nrow(dataset1))) {
  for (j in seq_len(nrow(dataset2))) {
    eval1 <- evaluate(
      har_eval_soft(3),                      # Configuração de avaliação
      #har_eval(),
      unlist(dataset1[i, ]),
      unlist(dataset2[j, ])
    )
    results1[i, j] <- eval1$tp_rate

    eval2 <- evaluate(
      har_eval(),
      unlist(dataset1[i, ]),
      unlist(dataset2[j, ])
    )
    results2[i, j] <- eval2$tp_rate
  }
}

############ Plotando #########

library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)

# Converter as matrizes para vetores
df <- tibble(
  F1 = c(as.vector(results1), as.vector(results2)),
  Method = rep(c("Soft", "Hard"), each = length(results1))
)

# Remover valores NA, se houver
df <- df %>% filter(!is.na(F1))

# Criar o boxplot
ggplot(df, aes(x = Method, y = F1, fill = Method)) +
  geom_boxplot() +
  labs(title = "Distribuição dos F1-scores",
       x = "Método de Avaliação",
       y = "F1-score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )


###################

library(tibble)
library(ggplot2)
library(dplyr)

# Calcular a diferença ponto a ponto
diff_f1 <- as.vector(results1 - results2)

# Remover valores NA
df_diff <- tibble(Difference = diff_f1) %>%
  filter(!is.na(Difference))

# Criar o boxplot da diferença
ggplot(df_diff, aes(x = "", y = Difference)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Diferença ponto a ponto entre Soft e Hard (F1-score)",
    x = "",
    y = "F1_soft - F1_hard"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

