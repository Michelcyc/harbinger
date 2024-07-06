# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(tidyr)

# Suponha que você tenha listas como essas (aqui, usando dados aleatórios para exemplo)
set.seed(123)
hard <- list(time = rnorm(1000, 0.12, 0.01), accuracy = rnorm(1000, 0.85, 0.05),
             precision = rnorm(1000, 0.80, 0.04), recall = rnorm(1000, 0.78, 0.03))
soft1 <- list(time = rnorm(1000, 0.10, 0.01), accuracy = rnorm(1000, 0.87, 0.04),
              precision = rnorm(1000, 0.82, 0.03), recall = rnorm(1000, 0.79, 0.02))
soft2 <- list(time = rnorm(1000, 0.11, 0.01), accuracy = rnorm(1000, 0.86, 0.05),
              precision = rnorm(1000, 0.81, 0.04), recall = rnorm(1000, 0.77, 0.03))

# Criar um dataframe a partir dessas listas
data <- bind_rows(
  as_tibble(hard) %>% mutate(Method = "Hard"),
  as_tibble(soft1) %>% mutate(Method = "Soft1"),
  as_tibble(soft2) %>% mutate(Method = "Soft2")
) %>% pivot_longer(-Method, names_to = "Metric", values_to = "Value")

# Verificar os dados
print(head(data))

# Criar o gráfico de caixa
ggplot(data, aes(x = Metric, y = Value, fill = Method)) +
  geom_boxplot() +
  labs(title = "Distribution of Metrics for Different Evaluation Methods",
       x = "Metrics", y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Pastel1") + # Escolha de cores
  guides(fill = guide_legend(title = "Method"))

# Exibir o gráfico
ggsave("metric_distribution.png", width = 12, height = 8, units = "in")
