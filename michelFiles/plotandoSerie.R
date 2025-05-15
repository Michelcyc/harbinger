# Carregar pacotes necessários
library(ggplot2)
library(readr)

library(tidyverse)

# Combine all datasets into one data frame with a new column 'dataset'
combined_data <- bind_rows(lapply(names(datasets), function(name) {
  data.frame(serie = datasets[[name]]$serie, dataset = name)
}))

# Create violin plots for each dataset
ggplot(combined_data, aes(x = dataset, y = serie)) +
  geom_violin(trim = FALSE, fill = "lightblue") +
  labs(title = "Violin Plots for Multiple Datasets",
       x = "Dataset",
       y = "Value of Series") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#outro


# Combine all datasets into one data frame with a new column 'dataset'
combined_data <- bind_rows(lapply(names(datasets), function(name) {
  data.frame(serie = datasets[[name]]$serie, dataset = name)
}))

# Create violin plots with jittered points to show outliers
ggplot(combined_data, aes(x = dataset, y = serie)) +
  geom_violin(trim = FALSE, fill = "red") +  # Violin plot
  geom_jitter(width = 0.2, size = 1, color = "grey", alpha = 0.1) +  # Jittered points to show all data, including outliers
  labs(title = "Violin Plots with Jittered Outliers for Multiple Datasets",
       x = "Dataset",
       y = "Value of Series") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#outro


# Function to identify outliers
is_outlier <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  return(x < (q1 - 3 * iqr) | x > (q3 + 3 * iqr))
}

# Combine all datasets into one data frame with a new column 'dataset' and identify outliers
combined_data <- bind_rows(lapply(names(datasets), function(name) {
  data <- data.frame(serie = datasets[[name]]$serie, dataset = name)
  data$outlier <- is_outlier(data$serie)
  return(data)
}))

# Create violin plots and plot only outliers using jitter
ggplot(combined_data, aes(x = dataset, y = serie)) +
  geom_violin(trim = FALSE, fill = "black") +  # Violin plot
  geom_jitter(data = combined_data %>% filter(outlier == TRUE),
              aes(x = dataset, y = serie),
              width = 0.2, size = 1.0, color = "grey", alpha = 0.6) +  # Jittered outliers
  labs(title = NULL,
       x = "Dataset",
       y = "Value of Series") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Increase number of Y-axis ticks
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

