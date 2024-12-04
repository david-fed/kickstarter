data <- read.csv("./data/kickstarter_projects.csv", encoding = "UTF-8")
# ----

library(dplyr)
library(ggplot2)

# Ensure the necessary columns are numeric
data <- data %>%
  mutate(
    Goal = as.numeric(Goal),
    Pledged = as.numeric(Pledged),
    State = tolower(State)  # Standardize State values
  )

# Calculate overshoot and undershoot for each project
data <- data %>%
  mutate(
    overshoot = ifelse(State == "successful", Pledged - Goal, NA),
    undershoot = ifelse(State != "successful", Goal - Pledged, NA)
  )

# Aggregate overshoot and undershoot per category
category_stats <- data %>%
  group_by(Category) %>%
  summarize(
    avg_overshoot = median(overshoot, na.rm = TRUE),
    avg_undershoot = median(undershoot, na.rm = TRUE),
    .groups = "drop"
  )

# Plot overshoot and undershoot per category
ggplot(category_stats, aes(x = reorder(Category, avg_overshoot))) +
  geom_bar(aes(y = avg_overshoot, fill = "Overshoot"), stat = "identity", alpha = 0.8) +
  geom_bar(aes(y = -avg_undershoot, fill = "Undershoot"), stat = "identity", alpha = 0.8) +
  scale_y_continuous(labels = abs) +  # Show absolute values on the y-axis
  coord_flip() +  # Flip axes for better readability
  theme_minimal() +
  labs(
    title = "Overshoot and Undershoot by Category",
    x = "Category",
    y = "Average Amount",
    fill = "Type"
  )


# ----

library(dplyr)
library(ggplot2)

# Data preparation: Calculate average goal amount and success rate by category
category_stats <- data %>%
  group_by(Category) %>%
  summarize(
    avg_goal = mean(Goal, na.rm = TRUE),
    success_rate = mean(State == "Successful"),
    .groups = "drop"
  )

# Calculate correlation
correlation <- cor(category_stats$success_rate, category_stats$avg_goal, use = "complete.obs", method = "spearman")

# Scatterplot: Success Rate vs. Goal Amount with Correlation
ggplot(category_stats, aes(x = success_rate, y = avg_goal, label = Category)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # Add a trendline
  geom_text(nudge_y = 0.02, size = 3, check_overlap = TRUE) +  # Add category labels
  scale_y_log10() +  # Log scale for goal amounts
  theme_minimal() +
  labs(
    title = "Success Rate vs. Average Goal Amount by Category",
    x = "Success Rate (Ratio)",
    y = "Average Goal Amount (Log Scale)",
    caption = "Data source: Kickstarter Dataset"
  ) +
  annotate(
    "text",
    x = 0.5,  # Adjust x position of the text as needed
    y = max(category_stats$avg_goal, na.rm = TRUE),  # Place the text at the top of the plot
    label = paste("Correlation:", round(correlation, 2)),
    size = 4, color = "darkred"
  )

# ----
library(dplyr)
library(lubridate)
library(plotly)

# Ensure the necessary columns are numeric and preprocess the dataset
data_modified <- data %>%
  mutate(
    Goal = as.numeric(Goal),
    Pledged = as.numeric(Pledged),
    State = State,
    Year = year(as.Date(Launched))  # Extract the year
  )

data_modified <- data_modified %>%
  filter(Year < 2018)

# Calculate overshoot and undershoot for each project
data_modified <- data_modified %>%
  mutate(
    overshoot = ifelse(State == "Successful", Pledged - Goal, NA),
    undershoot = ifelse(State != "Successful", Goal - Pledged, NA)
  )

# Aggregate overshoot and undershoot per category and year
category_year_stats <- data_modified %>%
  group_by(Year, Category) %>%
  summarize(
    avg_overshoot = median(overshoot, na.rm = TRUE),
    avg_undershoot = median(undershoot, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate cumulative overshoot and undershoot per category
cumulative_stats <- category_year_stats %>%
  group_by(Category) %>%
  summarize(
    total_overshoot = sum(avg_overshoot, na.rm = TRUE),
    .groups = "drop"
  )

# Reorder categories by cumulative overshoot (descending)
category_order <- cumulative_stats %>%
  arrange(desc(total_overshoot)) %>%
  pull(Category)

category_year_stats <- category_year_stats %>%
  mutate(Category = factor(Category, levels = category_order))

# Create a Plotly bar plot with a slider
fig <- category_year_stats %>%
  plot_ly(
    x = ~Category,
    y = ~avg_overshoot,
    frame = ~Year,
    type = "bar",
    name = "Overshoot",
    marker = list(color = 'rgba(44, 160, 44, 0.7)')
  ) %>%
  add_trace(
    y = ~-avg_undershoot,
    name = "Undershoot",
    marker = list(color = 'rgba(214, 39, 40, 0.7)')
  ) %>%
  layout(
    title = "Overshoot and Undershoot by Category Over Years",
    xaxis = list(title = "Category"),
    yaxis = list(title = "Average Amount (Median in USD)"),
    barmode = "overlay",
    legend = list(x = 0.9, y = 0.95)
  )

fig
