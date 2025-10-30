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


ggplot(df, aes(x = ed_complex, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Proporção de E+D de casos complexos", y = "Tempo de avaliação (s)") +
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



