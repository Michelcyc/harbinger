library(readr)

dataset <- read_csv('michelFiles/futuros_ibov_2019.csv')
names(dataset)[names(dataset) == "PreprocessedSeries"] <- "serie"
ibov <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_aus200_2019.csv')
aus200 <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_de30_2019.csv')
de30 <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_dj_2019.csv')
dj <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_esp35_2019.csv')
esp35 <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_hk50_2019.csv')
hk50 <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_jp225_2019.csv')
jp225 <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_nas_2019.csv')
nas <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_sp500_2019.csv')
sp500 <- head(dataset, 20000)

dataset <- read_csv('michelFiles/futuros_uk100_2019.csv')
uk100 <- head(dataset, 20000)

datasets <- list(ibov = ibov, aus200 = aus200, de30 = de30, dj = dj, esp35 = esp35,
                 hk50 = hk50, jp225 = jp225, nas = nas, sp500 = sp500, uk100 = uk100)

