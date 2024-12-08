library(arrow)
library(dplyr)
library(plotly)

# Read the Feather file
embeddings <- read_feather("id_pacmap_results.feather")

# Join with the full dataset
full_data <- read.csv("./data/kickstarter_projects.csv", encoding = "UTF-8")
merged_data <- merge(full_data, embeddings, by = "ID")

# Combine least frequent categories into "Other"
category_counts <- merged_data %>%
  count(Category, sort = TRUE)  # Count category occurrences

# Identify the 5 least frequent categories
least_frequent_categories <- tail(category_counts$Category, 5)

merged_data <- merged_data %>%
  mutate(Category = ifelse(Category %in% least_frequent_categories, "Other", Category))

# Sort the legend by Category frequency
sorted_categories <- merged_data %>%
  count(Category, sort = TRUE) %>%
  pull(Category)

merged_data$Category <- factor(merged_data$Category, levels = sorted_categories)

plot <- plot_ly(
  data = merged_data,
  x = ~UMAP_1,
  y = ~UMAP_2,
  split = ~Category,
  text = ~paste(
    "Title:", Name,
    "<br>Category:", Category,
    "<br>Subcategory:", Subcategory
  ),
  type = "scattergl",  # Use WebGL for performance
  mode = "markers",
  marker = list(size = 4, opacity = 0.5),
  hoverinfo = "text"  # Display custom text in hover box
) %>%
  layout(
    title = "2D Visualization of sampled Title Embeddings",
    xaxis = list(title = "Dimension 1"),
    yaxis = list(title = "Dimension 2"),
    legend = list(title = list(text = "Category"), traceorder = "normal")
  )

plot