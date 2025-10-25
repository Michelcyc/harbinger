# Load necessary library
library(ggplot2)

# Sample data for the plot
time <- seq(0, 10, length.out = 100)
value <- sin(time)  # Sine wave without noise for a smoother line

# Specific points for detection methods
time_points <- c(5, 6, 7)
value_points <- sin(time_points)  # Exact values on the sine wave

# Create a data frame for the line plot
data <- data.frame(time, value)

# Create a data frame for the points with labels for the legend
points_data <- data.frame(
  time = time_points,
  value = value_points,
  label = factor(c("Detection a", "Event", "Detection b"),
                 levels = c("Detection a", "Event", "Detection b")),  # Set the order of levels
  color = c("red", "blue", "darkred")
)

# Plot using ggplot2
ggplot(data, aes(x = time, y = value)) +
  geom_line(color = "black", size = 0.8) +   # Smooth line
  geom_point(data = points_data, aes(x = time, y = value, color = label), size = 3) +  # Smaller points
  geom_text(aes(x = 5, y = value_points[1] + 0.1, label = "a"), vjust = -1, size = 5,  family = "Times New Roman") +  # Label 'a' above Detection a
  geom_text(aes(x = 7, y = value_points[3] + 0.1, label = "b"), vjust = -1, size = 5, family = "Times New Roman") +  # Label 'b' above Detection b
  scale_color_manual(values = c("Detection a" = "red",
                                "Event" = "blue",
                                "Detection b" = "darkred")) +
  #labs(x = "time", y = NULL) +  # Label for X-axis, hide Y-axis label
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    panel.grid = element_blank(),  # Remove gridlines
    axis.title.x = element_text(face = "italic", size = 12),  # X-axis title with italic font
    axis.title.y = element_blank(),  # Hide Y-axis title
    axis.text.y = element_blank(),   # Hide Y-axis text
    axis.text.x = element_blank(),   # Hide X-axis text
    axis.ticks.y = element_blank(),  # Hide Y-axis ticks
    axis.line.x = element_line(color = "black", size = 0.5),  # X-axis line
    axis.line.y = element_blank(),  # No Y-axis line
    legend.position = "bottom",      # Position the legend below the plot
    legend.title = element_blank(),  # Remove legend title
    legend.direction = "horizontal", # Horizontal legend layout
    legend.spacing.x = unit(3, 'mm'),  # Reduce space between legend items
    legend.text = element_text(size = 13)  # Increase legend text size
  )
