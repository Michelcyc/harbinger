library(ggplot2)
library(dplyr)
library(tidyr)

# Load the .RData files
load("michelFiles/hard.RData")
load("michelFiles/soft1.RData")
load("michelFiles/soft2.RData")
load("michelFiles/soft3.RData")  # Adicionando o novo arquivo para SSED

hard_tempo <- sum(unlist(hard$time))
soft1_tempo <- sum(unlist(soft1$time))
soft2_tempo <- sum(unlist(soft2$time))
soft3_tempo <- sum(unlist(soft3$time))

# Função para limpar valores não-finitos
clean_values <- function(x) {
  x <- unlist(x)
  x[!is.finite(x)] <- NA  # Substitui infinitos/NaN por NA
  na.omit(x)              # Remove NAs
}

# NORMALIZAÇÃO (incluindo SSED)
combined_time <- c(
  clean_values(hard$time),
  clean_values(soft1$time),
  clean_values(soft2$time),
  clean_values(soft3$time)
)

min_time <- min(combined_time, na.rm = TRUE)
max_time <- max(combined_time, na.rm = TRUE)
normalize_time <- function(x) {
  (x - min_time) / (max_time - min_time)
}

# Preparar dados (incluindo SSED) com limpeza
hard <- list(
  accuracy = clean_values(hard$accuracy),
  precision = clean_values(hard$precision),
  recall = clean_values(hard$recall),
  time = normalize_time(clean_values(hard$time))
)

soft1 <- list(
  accuracy = clean_values(soft1$accuracy),
  precision = clean_values(soft1$precision),
  recall = clean_values(soft1$recall),
  time = normalize_time(clean_values(soft1$time))
)

soft2 <- list(
  accuracy = clean_values(soft2$accuracy),
  precision = clean_values(soft2$precision),
  recall = clean_values(soft2$recall),  # Note que corrigi para 'recall' (seu original tinha 'recall')
  time = normalize_time(clean_values(soft2$time))
)

soft3 <- list(
  accuracy = clean_values(soft3$accuracy),
  precision = clean_values(soft3$precision),
  recall = clean_values(soft3$recall),
  time = normalize_time(clean_values(soft3$time))
)

# Criar dataframe combinado (incluindo SSED)
data <- bind_rows(
  as_tibble(hard) %>% mutate(Method = "Hard"),
  as_tibble(soft1) %>% mutate(Method = "Soft Trad"),
  as_tibble(soft2) %>% mutate(Method = "Soft Par"),
  as_tibble(soft3) %>% mutate(Method = "SSED")
) %>% pivot_longer(-Method, names_to = "Metric", values_to = "Value")

# Definir ordem dos fatores (incluindo SSED)
data$Method <- factor(data$Method, levels = c("Hard", "Soft Trad", "Soft Par", "SSED"))

# Plot 1: Boxplot (incluindo SSED) sem warning
p <- ggplot(data, aes(x = Metric, y = Value, fill = Method)) +
  geom_boxplot(na.rm = TRUE) +  # Adicionado na.rm = TRUE para silenciar warnings
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1, size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "lines")) +
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = guide_legend(title = "Method"))

# Salvar o plot sem warnings
ggsave("metric_distribution.png", plot = p, width = 12, height = 8, units = "in")

# Plotar diretamente no RStudio
ggplot(data, aes(x = Metric, y = Value, fill = Method)) +
  geom_boxplot() +
  labs(title = "", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14, hjust = 1),  # Melhora legibilidade
        legend.position = "bottom",
        legend.text = element_text(size = 12)) +
  scale_fill_brewer(palette = "Pastel1", name = "Método:")
