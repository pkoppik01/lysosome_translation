
# Load the required library
library(ggplot2)

# Sample data in a data frame
medians_data <- data.frame(
  Group = c("BTA N40", "BTA N41", "PAB N40", "PAB B41"),
  Value = c(0.138492248, 2.93E-01, 0.147833192, 3.35E-01)
)

replicates_data <- data.frame(
  Group = rep(c("BTA N40", "BTA N41", "PAB N40", "PAB B41"), each = 5),
  Replicate = rep(1:5, times = 4),
  Value = c(
    0.136413764, 0.13809074, 0.134726584, 0.141419293, NA,
    2.81E-01, 3.06E-01, 2.86E-01, 2.99E-01, 2.94E-01,
    0.143347327, 0.150858939, 0.154335, 0.138200976, NA,
    3.45E-01, 3.40E-01, 3.37E-01, 3.63E-01, 3.04E-01
  )
)

# Create a bar plot with overlaying points
plot <- ggplot(medians_data, aes(x = Group, y = Value)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.5) +
  geom_point(data = replicates_data, aes(x = Group, y = Value), size = 2, color = "black") +
  labs(x = "Groups", y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot using the windows graphics device (for Windows)
windows()
print(plot)