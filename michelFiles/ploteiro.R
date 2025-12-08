library(dplyr); library(ggplot2)

#scale_x_log10() +
#scale_y_log10() +

df <- resumo_experimentos %>%
  mutate(
    event_plus_det  = num_deteccoes + num_eventos,
    p_ed_simple = 100*ed_simple / (ed_simple+ed_medium+ed_complex),
    p_ed_medium = 100*ed_medium / (ed_simple+ed_medium+ed_complex),
    p_ed_complex = 100*ed_complex / (ed_simple+ed_medium+ed_complex)
    )

## Plot de como tempo de avaliação sobe com o aumento do número de detecções e eventos no geral ##
ggplot(df, aes(x = event_plus_det, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(x = "Número de detecções e eventos", y = "Tempo de avaliação (s)") +
  theme_minimal()

ggplot(
  subset(df, ed_complex >= 1),
  aes(x = ed_complex, y = time_metric)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Número de detecções e eventos em casos complexos (log10)",
    y = "Tempo de avaliação (s, log10)"
  ) +
  theme_minimal()

ggplot(
  subset(df, ed_medium + ed_simple >= 1),
  aes(x = ed_medium + ed_simple, y = time_metric)
) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Número de detecções e eventos em casos simples + médios (log10)",
    y = "Tempo de avaliação (s, log10)"
  ) +
  theme_minimal()

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
  coord_flip() +
  labs(
    title = NULL,
    x = "Conjunto de dados",
    y = "Tamanho médio das séries"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11)
  )

library(scales)

ordem_datasets <- df_ed %>%
  distinct(dataset) %>%
  pull(dataset)

ggplot(df_media_tam,
       aes(x = factor(dataset, levels = ordem_datasets),
           y = media_tam_serie,
           fill = dataset)) +
  geom_col(alpha = 0.8, width = 0.7) +
  coord_flip() +
  labs(
    title = NULL,
    x = "Conjunto de dados",
    y = "Tamanho médio das séries (log₁₀)"
  ) +
  scale_y_continuous(
    trans = "log10",
    breaks = c(10, 100, 1000, 10000, 100000),
    labels = c("10", "100", "1000", "10000", "100000")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11)
  )


# Proporção de casos lineares
total   <- length(resumo_experimentos$linear_behavior)
n_true  <- sum(resumo_experimentos$linear_behavior, na.rm = TRUE)
prop    <- n_true / total


# Boxplot das proporcoes de eventos e deteccoes

library(dplyr)
library(ggplot2)

df_ed <- resumo_experimentos %>%
  mutate(
    ed_total   = num_deteccoes + num_eventos,
    ed_por_tam = ed_total / tam_serie
  )

ggplot(df_ed, aes(x = dataset, y = ed_por_tam)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  labs(
    x = "Conjuntos de dados",
    y = "Proporção do total de eventos e detecções por dataset",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(df_ed, aes(x = factor(dataset, levels = ordem_datasets),
                  y = ed_por_tam)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  labs(
    x = "Conjuntos de dados",
    y = "Densidade de eventos e detecções",
    title = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11),  # consistente com seus outros gráficos
    axis.text.x = element_text(size = 11)
  )


# Proporções de casos

library(dplyr)
library(tidyr)
library(ggplot2)

df_cases <- resumo_experimentos %>%
  group_by(dataset) %>%
  summarise(
    simple  = sum(n_cases_simple,  na.rm = TRUE),
    medium  = sum(n_cases_medium,  na.rm = TRUE),
    complex = sum(n_cases_complex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(simple, medium, complex),
    names_to  = "case_type",
    values_to = "n_cases"
  )

ggplot(df_cases, aes(x = dataset, y = n_cases, fill = case_type)) +
  geom_col(position = "fill") +  # "fill" => transforma em proporção
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    name   = "Tipo de caso",
    values = c("simple" = "#1b9e77", "medium" = "#7570b3", "complex" = "#d95f02"),
    labels = c("simple" = "Simples", "medium" = "Médio", "complex" = "Complexo")
  ) +
  labs(
    x = "Dataset",
    y = "Proporção de casos",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



## Bar plot dos casos lineares

df_lin <- resumo_experimentos %>%
  mutate(linear_behavior = factor(linear_behavior,
                                  levels = c(FALSE, TRUE),
                                  labels = c("Não linear", "Linear"))) %>%
  group_by(dataset, linear_behavior) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(dataset) %>%
  mutate(prop = n / sum(n))

ggplot(df_lin, aes(x = dataset, y = prop, fill = linear_behavior)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Dataset",
    y = "Proporção do comportamento",
    fill = "Comportamento",
    title = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
