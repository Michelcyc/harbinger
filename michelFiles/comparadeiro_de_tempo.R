suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(readr)
})

# ---- paths (ajuste o caminho se necessário)
paths <- c(
  smartsofted = "results/time_by_dataset_run_smartsofted.RData",
  softed      = "results/time_by_dataset_run_softed.RData",
  softedpar   = "results/time_by_dataset_run_softedpar.RData"
)

# ---- helper: carrega um .RData e retorna um data.frame com cols: dataset, total_time
load_time_df <- function(path) {
  e <- new.env(parent = emptyenv())
  objs <- load(path, envir = e)
  # tenta achar um objeto com nome "time_by_dataset"; se não existir, pega o primeiro data.frame
  obj_name <- if ("time_by_dataset" %in% objs) "time_by_dataset" else objs[1]
  df <- get(obj_name, envir = e)

  stopifnot(is.data.frame(df))

  # padroniza nomes: aceita "total_time", "time_sum", "sum_time_metric" etc.
  nm <- names(df)
  # coluna de dataset:
  if (!"dataset" %in% nm) {
    # tenta uma alternativa improvável (ajuste aqui se precisar)
    stop("Objeto não possui coluna 'dataset'. Verifique o conteúdo de ", path)
  }
  # coluna do tempo total (pega a primeira coluna numérica que não seja 'dataset')
  tempo_col <- setdiff(nm, "dataset")
  tempo_col <- tempo_col[ vapply(df[tempo_col], is.numeric, logical(1)) ]
  if (length(tempo_col) == 0) stop("Não encontrei coluna numérica de tempo em ", path)

  df |>
    select(dataset, !!tempo_col[1]) |>
    rename(total_time = !!tempo_col[1])
}

# ---- carrega e empilha (com nome do run)
all_runs <- bind_rows(
  lapply(names(paths), function(run) {
    load_time_df(paths[[run]]) |> mutate(run = run)
  }),
  .id = NULL
)

# se houver datasets faltando em algum run, completa com 0 (ou NA, se preferir)
all_runs <- all_runs |>
  complete(dataset, run, fill = list(total_time = 0))

# ordena os datasets pelo tempo total do run 'smartsofted' (mude se quiser outro critério)
ordem <- all_runs |>
  group_by(dataset) |>
  summarise(ref = sum(total_time[run == "smartsofted"], na.rm = TRUE)) |>
  arrange(desc(ref)) |>
  pull(dataset)

ggplot(all_runs, aes(x = factor(dataset, levels = ordem), y = total_time, fill = run)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  labs(
    title = "Tempo total por dataset (comparação entre runs)",
    x = "Dataset",
    y = "Tempo total (s)",
    fill = "Run"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

## Outro

library(dplyr)
library(ggplot2)
library(scales)

# Evita log(0)
eps <- 1e-6
all_runs_abs <- all_runs %>%
  mutate(total_time_eps = pmax(total_time, eps))

# Ordena datasets pelo tempo total somado (maior → menor)
ordem_abs <- all_runs_abs %>%
  group_by(dataset) %>%
  summarise(total = sum(total_time_eps, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(dataset)

ggplot(all_runs_abs, aes(x = factor(dataset, levels = ordem_abs),
                         y = total_time_eps, fill = run)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_y_log10(labels = label_number()) +
  labs(
    title = "Tempo total por dataset (escala log10)",
    x = "Dataset",
    y = "Tempo total (s, log10)",
    fill = "Run"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

# Outro 2

all_runs_rel <- all_runs %>%
  group_by(dataset) %>%
  mutate(time_rel = total_time / min(total_time, na.rm = TRUE)) %>%
  ungroup()

# Ordena datasets pelo pior (maior) fator relativo
ordem_rel <- all_runs_rel %>%
  group_by(dataset) %>%
  summarise(max_rel = max(time_rel, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(max_rel)) %>%
  pull(dataset)

ggplot(all_runs_rel, aes(x = factor(dataset, levels = ordem_rel),
                         y = pmax(time_rel, 1e-6), fill = run)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100),
                minor_breaks = NULL) +
  labs(
    title = "Tempo relativo por dataset (mais rápido = 1, escala log10)",
    x = "Dataset",
    y = "Fator × mais lento",
    fill = "Run"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())
