suppressPackageStartupMessages({
  library(readr)
})

# ----------------- Config -----------------
base_dir <- "michelFiles"
out_rda  <- file.path(base_dir, "datasets_indices.RData")
n_max    <- 20000L

# nome do índice -> nome do arquivo
files <- c(
  ibov   = "futuros_ibov_2019.csv",
  aus200 = "futuros_aus200_2019.csv",
  de30   = "futuros_de30_2019.csv",
  dj     = "futuros_dj_2019.csv",
  esp35  = "futuros_esp35_2019.csv",
  hk50   = "futuros_hk50_2019.csv",
  jp225  = "futuros_jp225_2019.csv",
  nas    = "futuros_nas_2019.csv",
  sp500  = "futuros_sp500_2019.csv",
  uk100  = "futuros_uk100_2019.csv"
)
files <- file.path(base_dir, files)
names(files) <- c("ibov","aus200","de30","dj","esp35","hk50","jp225","nas","sp500","uk100")

# ------------- Helpers --------------------

# Converte "Classe" para lógico robustamente
coerce_event <- function(x) {
  if (is.logical(x)) return(replace(x, is.na(x), FALSE))
  if (is.numeric(x)) return(!is.na(x) & x != 0)
  if (is.factor(x))  x <- as.character(x)
  if (is.character(x)) {
    xl <- tolower(trimws(x))
    return(xl %in% c("1","true","t","y","yes"))
  }
  rep(FALSE, length(x))
}

# Lê um CSV e cria data.frame(value, event)
read_index_df <- function(path, n_max = 20000L) {
  if (!file.exists(path)) stop("Arquivo não encontrado: ", path)

  df <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  if (nrow(df) == 0) stop("Arquivo vazio: ", path)

  # limita linhas
  if (!is.null(n_max) && is.finite(n_max) && nrow(df) > n_max) {
    df <- head(df, n_max)
  }

  # alguns arquivos usam "PreprocessedSeries" -> normaliza para "serie"
  names(df)[names(df) == "PreprocessedSeries"] <- "serie"

  if (!("serie" %in% names(df)))
    stop("Coluna 'serie' não encontrada em: ", path, " (colunas: ", paste(names(df), collapse=", "), ")")
  if (!("Classe" %in% names(df)))
    stop("Coluna 'Classe' não encontrada em: ", path, " (colunas: ", paste(names(df), collapse=", "), ")")

  value <- suppressWarnings(as.numeric(df$serie))
  event <- coerce_event(df$Classe)

  data.frame(value = value, event = event, check.names = FALSE)
}

# ------------- Construção -----------------

datasets_indices <- setNames(vector("list", length(files)), names(files))

for (nm in names(files)) {
  cat("Lendo", nm, "de", files[[nm]], "...\n")
  datasets_indices[[nm]] <- read_index_df(files[[nm]], n_max = n_max)
}

# ------------- Salvamento -----------------

dir.create(dirname(out_rda), showWarnings = FALSE, recursive = TRUE)
save(datasets_indices, file = out_rda, compress = "xz")
cat("\nSalvo em:", normalizePath(out_rda), "\n")
