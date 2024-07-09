# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the .RData files
load("michelFiles/hard.RData")
load("michelFiles/soft1.RData")
load("michelFiles/soft2.RData")

mean_time_hard <- mean(unlist(hard$time))
mean_time_soft1 <- mean(unlist(soft1$time))
mean_time_soft2 <- mean(unlist(soft2$time))

# NORMALIZING
combined_time <- c(unlist(hard$time), unlist(soft1$time), unlist(soft2$time))

# Step 2: Create a normalization function based on the combined time data
min_time <- min(combined_time)
max_time <- max(combined_time)
normalize_time <- function(x) {
  (x - min_time) / (max_time - min_time)
}

hard <- list(
  accuracy = unlist(hard$accuracy),
  precision = unlist(hard$precision),
  recall = unlist(hard$recall),
  time = normalize_time(unlist(hard$time))
)
soft1 <- list(
  accuracy = unlist(soft1$accuracy),
  precision = unlist(soft1$precision),
  recall = unlist(soft1$recall),
  time = normalize_time(unlist(soft1$time))
)
soft2 <- list(
  accuracy = unlist(soft2$accuracy),
  precision = unlist(soft2$precision),
  recall = unlist(soft2$recall),
  time = normalize_time(unlist(soft2$time))
)

# Create a combined data frame
data <- bind_rows(
  as_tibble(hard) %>% mutate(Method = "Hard"),
  as_tibble(soft1) %>% mutate(Method = "Soft 1"),
  as_tibble(soft2) %>% mutate(Method = "Soft 2")
) %>% pivot_longer(-Method, names_to = "Metric", values_to = "Value")

# Plot setup with no legends
ggplot(data, aes(x = Metric, y = Value, fill = Method)) +
  geom_boxplot() +
  labs(title = NULL,
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, size = 14),  # Adjust text angle for better readability
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),  # Increase and bold the legend title font size
        legend.text = element_text(size = 12),  # Increase the legend text font size
        legend.key.size = unit(1.5, "lines")) +  # Increase the size of the legend keys
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = guide_legend(title = "Método"))

# Save the plot
ggsave("metric_distribution.png", width = 12, height = 8, units = "in")


#   Plotagem 2
# Instalar pacotes necessários, se ainda não estiverem instalados
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(reshape2)) install.packages("reshape2")

# Carregar pacotes
library(ggplot2)
library(reshape2)

# Dados
data <- data.frame(
  Metrica = c("acurácia", "precisão", "revocação", "F1"),
  Metrica_dura = c(0.96, 0.33, 0.33, 0.33),
  SoftED_original = c(0.97, 0.63, 0.63, 0.63),
  SoftED_expandido = c(0.98, 0.76, 0.76, 0.76)
)

# Converter dados para o formato longo
data_melted <- melt(data, id.vars = "Metrica")

# Plot
ggplot(data_melted, aes(x = Metrica, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = value), vjust = 1.6, color = "white", position = position_dodge(0.9), size = 4, fontface = "bold") +
  labs(fill = "Método") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),  # Aumentar tamanho do texto
    axis.title.x = element_blank(),  # Remover título do eixo x
    axis.title.y = element_blank(),  # Remover título do eixo y
    axis.text = element_text(size = 12),  # Aumentar tamanho do texto dos eixos
    legend.title = element_text(size = 15),  # Aumentar tamanho do título da legenda
    legend.text = element_text(size = 12)  # Aumentar tamanho do texto da legenda
  ) +
  scale_fill_manual(values = c("Metrica_dura" = "lightblue", "SoftED_original" = "lightcoral", "SoftED_expandido" = "lightgreen"))  # Cores pastel
