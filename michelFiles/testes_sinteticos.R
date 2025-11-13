remove.packages("harbinger")
quit(save = "no")
devtools::install_github("Michelcyc/harbinger@minha-trilha-nova", force = TRUE, upgrade = "never")

suppressPackageStartupMessages({
  library(daltoolbox)
  library(daltoolboxdp)
  library(tspredit)
  library(harbinger)
  library(united)
})

generate_event_detection <- function(n,
                                     frac,          # fração (0–1) de eventos/detecções
                                     jitter_window = 60) {
  if (n <= 0) stop("n must be positive")
  if (frac < 0) stop("frac must be non-negative")

  # número de eventos/detecções
  n_events <- round(n * frac)
  if (frac > 0 && n_events < 1L) {
    n_events <- 1L
  }
  if (frac == 0) {
    n_events <- 0L
  }

  event     <- rep(FALSE, n)
  detection <- rep(FALSE, n)

  if (n_events == 0L) {
    return(list(
      n         = n,
      frac      = frac,
      event     = event,
      detection = detection
    ))
  }

  # Eventos: posições uniformes
  event_idx <- sort(sample.int(n, n_events, replace = FALSE))
  event[event_idx] <- TRUE

  # Detecções: concentradas ao redor dos eventos
  offsets <- sample.int(2L * jitter_window + 1L, n_events, replace = TRUE) -
    (jitter_window + 1L)  # em [-jitter_window, +jitter_window]

  raw_det_idx <- event_idx + offsets
  raw_det_idx[raw_det_idx < 1L] <- 1L
  raw_det_idx[raw_det_idx > n]  <- n

  det_idx <- unique(raw_det_idx)

  # garante mesmo número de detecções que de eventos
  if (length(det_idx) > n_events) {
    det_idx <- det_idx[seq_len(n_events)]
  } else if (length(det_idx) < n_events) {
    needed    <- n_events - length(det_idx)
    remaining <- setdiff(seq_len(n), det_idx)
    det_idx   <- c(det_idx, sample(remaining, needed))
  }

  detection[det_idx] <- TRUE

  list(
    n         = n,
    frac      = frac,
    event     = event,
    detection = detection
  )
}

##########
## GRID ##
##########
library(tibble)

# tamanhos: 10^2, 10^3, ..., 10^8
series_sizes <- as.integer(10^(2:8))

# porcentagens: 0.1%, 0.2%, 0.4%, ..., 25.6%
percents  <- 0.1 * 2^(0:8)   # em %
fractions <- percents / 100  # em fração 0–1

grid <- expand.grid(
  n    = series_sizes,
  frac = fractions
)

##########
## Tibble ##
##########

sim_list <- lapply(seq_len(nrow(grid)), function(i) {
  generate_event_detection(
    n    = grid$n[i],
    frac = grid$frac[i],
    jitter_window = 60
  )
})

sim_df <- tibble(
  n         = vapply(sim_list, `[[`, numeric(1), "n"),
  frac      = vapply(sim_list, `[[`, numeric(1), "frac"),
  event     = lapply(sim_list, `[[`, "event"),
  detection = lapply(sim_list, `[[`, "detection")
)

sim_df

########
# Teste#
########

k <- 1  # escolhe uma combinação

serie <- list(
  event     = sim_df$event[[k]],
  detection = sim_df$detection[[k]]
)

avaliacao_soft <- evaluate(
  har_eval_soft(sw_size = 30),
  serie$event,
  serie$detection
)

#########################
### TESTE COMPLETO ######
#########################

library(dplyr)

# valores únicos de n e frac
n_vals    <- sort(unique(sim_df$n))
frac_vals <- sort(unique(sim_df$frac))

# matriz de tempos (em segundos)
time_mat <- matrix(
  NA_real_,
  nrow = length(n_vals),
  ncol = length(frac_vals),
  dimnames = list(
    paste0("n_", n_vals),
    paste0("frac_", frac_vals)
  )
)

### Limite de tempo em segundos (você pode mudar aqui)
timeout_limit <- 60

### Etapa 2 ## Loop para medir o tempo de execução
for (i in seq_along(n_vals)) {
  for (j in seq_along(frac_vals)) {
    n_ij    <- n_vals[i]
    frac_ij <- frac_vals[j]

    # acha a linha correspondente em sim_df
    idx <- which(sim_df$n == n_ij & sim_df$frac == frac_ij)

    if (length(idx) != 1L) {
      warning("Esperava exatamente 1 combinação para n = ", n_ij,
              " e frac = ", frac_ij, " mas encontrei ", length(idx))
      next
    }

    serie_event     <- sim_df$event[[idx]]
    serie_detection <- sim_df$detection[[idx]]

    # mede o tempo de execução da métrica com limite
    t_exec <- tryCatch(
      {
        # define limite de tempo só para esta avaliação
        setTimeLimit(elapsed = timeout_limit, transient = TRUE)

        tempo <- system.time(
          evaluate(
            har_eval_soft(sw_size = 30),
            serie_event,
            serie_detection
          )
        )["elapsed"]

        # reseta o limite (importante!)
        setTimeLimit(elapsed = Inf, transient = TRUE)

        as.numeric(tempo)
      },
      error = function(e) {
        # se estourou o tempo, registra o limite
        msg <- conditionMessage(e)
        if (grepl("reached elapsed time limit", msg)) {
          # reseta o limite e retorna o timeout
          setTimeLimit(elapsed = Inf, transient = TRUE)
          timeout_limit
        } else {
          # outro erro qualquer: opcionalmente você pode logar NA
          setTimeLimit(elapsed = Inf, transient = TRUE)
          NA_real_
        }
      }
    )

    time_mat[i, j] <- t_exec
  }
}


## Etapa 3 - Formato LONG para plotar

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 1) Converter a matriz em data frame "long"
time_df <- as.data.frame(time_mat) %>%
  mutate(n = as.numeric(sub("n_", "", rownames(time_mat)))) %>%
  pivot_longer(
    cols      = starts_with("frac_"),
    names_to  = "frac_label",
    values_to = "time_sec"
  ) %>%
  mutate(
    frac = as.numeric(sub("frac_", "", frac_label))
  ) %>%
  filter(!is.na(time_sec))  # garante que não entra NA no heatmap


# Heatmap #

ggplot(time_df, aes(x = n, y = frac, fill = time_sec)) +
  geom_tile() +
  scale_x_log10(
    breaks = sort(unique(time_df$n)),
    labels = label_number(accuracy = 1)  # mostra 100, 1000, 10000...
  ) +
  scale_y_continuous(
    breaks = sort(unique(time_df$frac)),
    labels = percent_format(accuracy = 0.1)  # mostra 0.1%, 0.2%, ...
  ) +
  scale_fill_viridis_c(
    option = "plasma",  # pode trocar: "magma", "viridis", etc.
    name   = "Tempo (s)"
  ) +
  labs(
    x = "Tamanho da série (n)",
    y = "Proporção de eventos/detecções",
    title = "Tempo de execução da métrica por tamanho de série e proporção de eventos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank()
  )

# Para eixo y invertido
#+ scale_y_reverse(
#  breaks = sort(unique(time_df$frac)),
#  labels = percent_format(accuracy = 0.1)
#)


########## Tentativa 2 #############

# LONG
time_df <- as.data.frame(time_mat) %>%
  mutate(n = as.numeric(sub("n_", "", rownames(time_mat)))) %>%
  pivot_longer(
    cols      = starts_with("frac_"),
    names_to  = "frac_label",
    values_to = "time_sec"
  ) %>%
  mutate(
    frac = as.numeric(sub("frac_", "", frac_label))
  ) %>%
  filter(!is.na(time_sec))

# Heatmap com x em log10 e y em log2
ggplot(time_df, aes(x = n, y = frac, fill = time_sec)) +
  geom_tile() +
  scale_x_log10(
    breaks = sort(unique(time_df$n)),
    labels = label_number(accuracy = 1)
  ) +
  scale_y_continuous(
    trans  = log2_trans(),                          # eixo em log2
    breaks = sort(unique(time_df$frac)),            # usa exatamente as tuas frações
    labels = percent_format(accuracy = 0.1)         # mostra 0.1%, 0.2%, ...
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name   = "Tempo (s)"
  ) +
  labs(
    x = "Tamanho da série (n, log10)",
    y = "Proporção de eventos/detecções (log2)",
    title = "Tempo de execução da métrica por tamanho de série e proporção de eventos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank()
  )

