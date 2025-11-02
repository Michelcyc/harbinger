
#### BOXPLOT PRECISION RECALL E F1 ##########

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

########## REGISTRO DE TEMPO DE EXECUCAO #############
suppressPackageStartupMessages({
  library(dplyr)
})

# Rótulo deste “run” para compor o nome do arquivo
#run_label <- "run_softed"
run_label <- "run_softedpar"
#run_label <- "run_smartsofted"

time_by_dataset <- resumo_experimentos %>%
  group_by(dataset) %>%
  summarise(
    total_time_metric = sum(time_metric, na.rm = TRUE),
    n_series          = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_time_metric))

# Cria pasta e salva o objeto para uso posterior
dir.create("results", showWarnings = FALSE, recursive = TRUE)
save(
  time_by_dataset,
  file = file.path("results", paste0("time_by_dataset_", run_label, ".RData")),
  compress = "xz"
)

# (opcional) imprimir um preview
print(time_by_dataset, n = Inf)

