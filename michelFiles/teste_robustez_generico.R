remove.packages("harbinger")
quit(save = "no")
library("daltoolbox")
devtools::install_github("Michelcyc/harbinger", force=TRUE, upgrade="never")
library("harbinger")

#install.packages('RcppHungarian')
#library(RcppHungarian)

library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)

# Gerar todos os vetores possíveis de 7 valores lógicos (TRUE/FALSE)
dataset1 <- expand.grid(replicate(9, c(TRUE, FALSE), simplify = FALSE))
dataset2 <- expand.grid(replicate(9, c(TRUE, FALSE), simplify = FALSE))

# Inicializar matriz para armazenar F1-scores
results1 <- matrix(NA, nrow = nrow(dataset1), ncol = nrow(dataset2))
results2 <- matrix(NA, nrow = nrow(dataset1), ncol = nrow(dataset2))

k=3
k_message <- sprintf("K = %s", k)


# Loop de avaliação: comparar cada linha de dataset1 com cada linha de dataset2
for (i in seq_len(nrow(dataset1))) {
  for (j in seq_len(nrow(dataset2))) {
    eval1 <- evaluate(
      har_eval_soft(k),                      # Configuração de avaliação
      unlist(dataset1[i, ]),
      unlist(dataset2[j, ])
    )
    results1[i, j] <- eval1$Ts

    eval2 <- evaluate(
      har_eval(),
      #har_eval_soft(1),
      unlist(dataset1[i, ]),
      unlist(dataset2[j, ])
    )
    results2[i, j] <- eval2$Ts
  }
}

############ Plotando #########
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
    title = paste("Diferença ponto a ponto", k_message),
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24, face = "bold"),
  )


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
  labs(title = paste("Boxplot ", k_message),
       x = "",
       y = "") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24, face = "bold"),
    legend.position = "none"
  )

