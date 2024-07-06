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

datasets <- list(ibov = ibov, aus200 = aus200, de30 = de30, dj = dj, esp35 = esp35)
