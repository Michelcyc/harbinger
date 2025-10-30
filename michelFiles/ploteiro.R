library(dplyr); library(ggplot2)

df <- resumo_experimentos %>%
  mutate(
    total_casos     = n_cases_simple + n_cases_medium + n_cases_complex,
    event_plus_det  = num_deteccoes + num_eventos,
    prop_simples    = n_cases_simple  / pmax(total_casos, 1),
    prop_medios     = n_cases_medium  / pmax(total_casos, 1),
    prop_complexos  = n_cases_complex / pmax(total_casos, 1),
    ratio_det_event = num_deteccoes / pmax(num_eventos, 1),
    over_under_det  = num_deteccoes - num_eventos,  # >0: superdetecção; <0: sub
    comp_index      = (1*n_cases_simple + 2*n_cases_medium + 3*n_cases_complex) / pmax(total_casos, 1)
  )

# Relação entre tempo e casos complexos?
# Precisa ser entre tempo e quantidade de D+E em casos complexos
ggplot(df, aes(x = prop_complexos, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(x = "Proporção de casos complexos", y = "Tempo de avaliação (s)") +
  theme_minimal()

#Tempo e total ocorrências (deteccoes + eventos)
ggplot(df, aes(x = event_plus_det, y = time_metric)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Total de casos (S+M+C)", y = "Tempo de avaliação (s)") +
  theme_minimal()

#Densidade de tempo por linearidade
ggplot(df, aes(x = time_metric, fill = linear_behavior)) +
  geom_density(alpha = .4) +
  labs(x = "Tempo de avaliação (s)", fill = "Linearidade") +
  theme_minimal()

ggplot(df, aes(x = time_metric, fill = linear_behavior)) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  scale_x_log10() +
  labs(x = "Tempo de avaliação (s, escala log)", fill = "Linearidade") +
  theme_minimal()

#CDF (ECDF) do tempo por linearidade
ggplot(df, aes(x = time_metric, color = linear_behavior)) +
  stat_ecdf(geom = "step") +
  labs(x = "Tempo de avaliação (s)", y = "Fração acumulada", color = "Linearidade") +
  theme_minimal()



