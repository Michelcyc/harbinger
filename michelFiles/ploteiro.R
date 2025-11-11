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
ggplot(subset(df, tam_serie >= 1), aes(x = tam_serie, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Número de detecções + eventos (escala log10)",
    y = "Tempo de avaliação (s, escala log10)"
  ) +
  theme_minimal()

#com log
ggplot(subset(df, event_plus_det >= 0), aes(x = event_plus_det, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Tamanho da série (escala log10)",
    y = "Tempo de avaliação (s, escala log10)"
  ) +
  theme_minimal()


ggplot(subset(df, ed_complex >= 1), aes(x = ed_complex, y = time_metric)) +
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

# simple e medios
df2 <- df %>%
  mutate(
    p_ed_simple_0 = coalesce(p_ed_simple, 0),
    p_ed_medium_0 = coalesce(p_ed_medium, 0)
  )

ggplot(df2, aes(x = p_ed_simple_0 + p_ed_medium_0, fill = linear_behavior)) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  labs(x = "Ocorrências em casos simples e médios", fill = "Linearidade") +
  theme_minimal()


df2 <- df %>%
  mutate(
    p_ed_complex_pct = coalesce(p_ed_complex, 0) * 100
  )

ggplot(
  df2,
  aes(
    x = p_ed_complex_pct + 0.01,   # evita log(0)
    fill = linear_behavior
  )
) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  scale_x_log10() +
  labs(
    x = "Porcentagem de ocorrências em casos complexos (%, log10)",
    fill = "Linearidade"
  ) +
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

ggplot(df_media_tam, aes(x = reorder(dataset, media_tam_serie),
                         y = media_tam_serie, fill = dataset)) +
  geom_col(alpha = 0.8, width = 0.7) +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Média do tamanho das séries por dataset",
    x = "Dataset",
    y = "Tamanho médio das séries (log10)"
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

# MÉDIA DE EVENTOS POR DATASET #
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# Calcula a média de eventos por dataset
df_media_eventos <- resumo_experimentos %>%
  distinct(dataset, series, num_eventos, .keep_all = TRUE) %>%
  group_by(dataset) %>%
  summarise(media_eventos = mean(num_eventos, na.rm = TRUE)) %>%
  arrange(desc(media_eventos))

# Cria o gráfico de barras
ggplot(df_media_eventos, aes(x = reorder(dataset, media_eventos),
                             y = media_eventos, fill = dataset)) +
  geom_col(alpha = 0.8, width = 0.7) +
  coord_flip() +
  scale_y_log10() +  # Escala logarítmica opcional
  labs(
    title = "Média de eventos por dataset",
    x = "Dataset",
    y = "Média de eventos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11)
  )
