# Carregar pacotes necessários
library(ggplot2)
library(readr)

# Carregar o arquivo CSV
data <- read_csv('michelFiles/futuros_ibov_2019.csv')

# Selecionar as primeiras 1000 observações
data_subset <- data[1:1000, ]

# Plotar a série temporal com as primeiras 1000 observações
ggplot(data_subset, aes(x = 1:nrow(data_subset), y = PreprocessedSeries)) +
  geom_line(color = "blue") +
  labs(title = NULL,
       x = "Índice",
       y = "Valor da Série") +
  theme_minimal()
