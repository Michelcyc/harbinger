# Load necessary library
library(ggplot2)

# Sample data for the plot
time <- seq(0, 10, length.out = 100)
value <- sin(time) + rnorm(100, sd = 0.2)

# Specific points for detection methods
time_points <- c(3, 6, 8)
value_points <- sin(time_points) + c(-0.1, 0.2, -0.15)

# Create a data frame for the line plot
data <- data.frame(time, value)

# Create a data frame for the points
points_data <- data.frame(
  time = time_points,
  value = value_points,
  label = c("Detection Method B", "Detection Method A", "Some Point"),
  color = c("purple", "red", "blue")
)

# Plot using ggplot2
ggplot(data, aes(x = time, y = value)) +
  geom_line(color = "black", size = 0.8) +   # Smooth line
  geom_point(data = points_data, aes(x = time, y = value, color = color), size = 4) +
  geom_text(data = points_data, aes(x = time, y = value, label = label, color = color),
            vjust = -1, hjust = 0, size = 4, show.legend = FALSE) +
  scale_color_manual(values = c("purple" = "purple", "red" = "red", "blue" = "blue")) +
  theme_minimal() +
  labs(x = "time", y = NULL) +
  annotate("text", x = c(3, 6, 8), y = min(value) - 0.3,
           label = c("t - k2\n(k2 > k1)", "t", "t + k1"), size = 4) +
  theme(
    axis.title.x = element_text(face = "italic", size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

