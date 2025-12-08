######################################################
########## REGISTRO DE TEMPO DE EXECUCAO #############
######################################################

suppressPackageStartupMessages({
  library(dplyr)
})

# Rótulo deste “run” para compor o nome do arquivo
#run_label <- "run_softed"
#run_label <- "run_softedpar"
run_label <- "run_smartsofted"

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


### Comparadeiro mesmo ###

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
#Troquei por "softed", era "smartsofted"
ordem <- all_runs |>
  group_by(dataset) |>
  summarise(ref = sum(total_time[run == "softed"], na.rm = TRUE)) |>
  arrange(desc(ref)) |>
  pull(dataset)

## Comparando tempo ##

library(dplyr)
library(ggplot2)
library(scales)

all_runs_rel <- all_runs %>%
  group_by(dataset) %>%
  mutate(time_rel = total_time / min(total_time, na.rm = TRUE)) %>%
  ungroup()

ordem_rel <- all_runs_rel %>%
  group_by(dataset) %>%
  summarise(max_rel = max(time_rel, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(max_rel)) %>%
  pull(dataset)

ggplot(all_runs_rel,
       aes(x = factor(dataset, levels = ordem_rel),
           y = pmax(time_rel, 1e-6),
           fill = run)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_y_continuous(
    trans  = log2_trans(),
    breaks = c(1, 2, 4, 8, 16, 32, 64, 128, 256)
  ) +
  labs(
    x     = "Conjunto de dados",
    y     = "Multiplicador de tempo (relativo ao mais rápido)",
    fill  = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 12),
    legend.title       = element_text(size = 12)
  )

#### Média de tempo de execução ####

# Calcula a média do tempo total por métrica (run)
medias <- all_runs %>%
  group_by(run) %>%
  summarise(media_tempo = mean(total_time, na.rm = TRUE), .groups = "drop")

# Imprime no formato desejado
for (i in seq_len(nrow(medias))) {
  cat(sprintf("Média do tempo de %s: %.6f\n",
              medias$run[i],
              medias$media_tempo[i]))
}

# Casos atípicos

datasets_desejados <- c(
  "A2Benchmark",
  "nab_realTraffic",
  "A1Benchmark",
  "nab_realAdExchange"
)

tempos_especificos <- all_runs %>%
  filter(dataset %in% datasets_desejados)

print(tempos_especificos, n = Inf)

medias_especificas <- all_runs %>%
  filter(dataset %in% datasets_desejados) %>%
  group_by(run) %>%
  summarise(media_tempo = mean(total_time, na.rm = TRUE), .groups = "drop")

# Imprimir no formato desejado
for (i in seq_len(nrow(medias_especificas))) {
  cat(sprintf("Média do tempo de %s nos datasets selecionados: %.6f\n",
              medias_especificas$run[i],
              medias_especificas$media_tempo[i]))
}
