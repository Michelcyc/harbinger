library(dplyr); library(ggplot2)

df <- resumo_experimentos %>%
  mutate(
    event_plus_det  = num_deteccoes + num_eventos,
    p_ed_simple = ed_simple / (ed_simple+ed_medium+ed_complex),
    p_ed_medium = ed_medium / (ed_simple+ed_medium+ed_complex),
    p_ed_complex = ed_complex / (ed_simple+ed_medium+ed_complex)
    )


ggplot(df, aes(x = event_plus_det, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(x = "Número de detecções e eventos", y = "Tempo de avaliação (s)") +
  theme_minimal()

#com log
ggplot(df, aes(x = event_plus_det + 0.001, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Número de detecções + eventos (escala log10)",
    y = "Tempo de avaliação (s, escala log10)"
  ) +
  theme_minimal()



ggplot(df, aes(x = ed_complex, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Proporção de E+D de casos complexos", y = "Tempo de avaliação (s)") +
  theme_minimal()

#com log
ggplot(df, aes(x = ed_complex + 0.001, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "E+D de casos complexos (log10)",
    y = "Tempo de avaliação (s, log10)"
  ) +
  theme_minimal()


#Densidade de tempo por linearidade
ggplot(df, aes(x = time_metric, fill = linear_behavior)) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  scale_x_log10() +
  labs(x = "Tempo de avaliação (s, escala log)", fill = "Linearidade") +
  theme_minimal()

ggplot(df, aes(x = (p_ed_simple + p_ed_medium), fill = linear_behavior)) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  scale_x_log10() +
  labs(x = "Ocorrências em casos simples e médios", fill = "Linearidade") +
  theme_minimal()

ggplot(df, aes(x = p_ed_complex, fill = linear_behavior)) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  scale_x_log10() +
  labs(x = "Porcentagem de ocorrências em casos complexos", fill = "Linearidade") +
  theme_minimal()

#Densidade de tempo por linearidade
ggplot(df, aes(x = ed_complex, fill = linear_behavior)) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  scale_x_log10() +
  labs(x = "Tempo de avaliação (s, escala log)", fill = "Linearidade") +
  theme_minimal()

#CDF (ECDF) do ocorrencia casos complexos por linearidade
ggplot(df, aes(x = time_metric, color = linear_behavior)) +
  stat_ecdf(geom = "step") +
  labs(x = "Tempo de avaliação (s)", y = "Fração acumulada", color = "Linearidade") +
  theme_minimal()

#TAMANHO DAS SÉRIES DOS DATASETS #
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# Calcula a média dos tamanhos das séries por dataset
df_media_tam <- resumo_experimentos %>%
  distinct(dataset, series, tam_serie, .keep_all = TRUE) %>%
  group_by(dataset) %>%
  summarise(media_tam_serie = mean(tam_serie, na.rm = TRUE)) %>%
  arrange(desc(media_tam_serie))

# Cria o gráfico de barras
ggplot(df_media_tam, aes(x = reorder(dataset, media_tam_serie), y = media_tam_serie, fill = dataset)) +
  geom_col(alpha = 0.8, width = 0.7) +
  coord_flip() +  # deixa as barras horizontais (mais legível se tiver muitos datasets)
  labs(
    title = "Média do tamanho das séries por dataset",
    x = "Dataset",
    y = "Tamanho médio das séries"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11)
  )

## Tamanho das deteccoes mais eventos dos datasets ###

# MÉDIA DE (EVENTOS + DETECÇÕES) POR DATASET #
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# Calcula a média de (eventos + detecções) por dataset
df_media_ed <- resumo_experimentos %>%
  distinct(dataset, series, num_eventos, num_deteccoes, .keep_all = TRUE) %>%
  mutate(event_plus_det = num_eventos + num_deteccoes) %>%
  group_by(dataset) %>%
  summarise(media_event_plus_det = mean(event_plus_det, na.rm = TRUE)) %>%
  arrange(desc(media_event_plus_det))

# Cria o gráfico de barras
ggplot(df_media_ed, aes(x = reorder(dataset, media_event_plus_det),
                        y = media_event_plus_det, fill = dataset)) +
  geom_col(alpha = 0.8, width = 0.7) +
  coord_flip() +
  scale_y_log10() +  # Escala logarítmica (opcional — ajuda se houver grande variação)
  labs(
    title = "Média de (detecções + eventos) por dataset",
    x = "Dataset",
    y = "Média de (detecções + eventos)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11)
  )


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

