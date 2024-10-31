kickstarter <- read.csv("./data/kickstarter_projects.csv", encoding = "UTF-8")
## Grafik 1 ----
category.relative.frequency <- table(kickstarter$Category) / length(kickstarter$Category) * 100

par(mar=c(8, 6, 4, 2))
category.barplot <- barplot(
  sort(category.relative.frequency, decreasing = TRUE),
  las = 2,
  main = "Amount of Kickstarter projects by category",
  col = "#05ce78",
  ylim = c(0, 20),
  border = "white"
)
title(xlab="Category", line=6)
title(ylab="Relative frequency (%)", line=3)
text(
  x = category.barplot,
  y = sort(category.relative.frequency, decreasing = TRUE) + 0.5,
  labels = round(
    sort(category.relative.frequency,
         decreasing = TRUE
    ),
    1)
)

# Mean relative frequency abline
abline(h = mean(category.relative.frequency), col = "red", lty = 2, lwd = 2)
text(x = max(category.barplot), y = mean(category.relative.frequency), labels = paste("Mean:", round(mean(category.relative.frequency), 2)), pos = 3, col = "red")

## Grafik 2 ----
library(plotly)
library(countrycode)

country.freq <- table(kickstarter$Country)
country.rel.freq <- round(country.freq / length(kickstarter$Country) * 100, digits = 2)

# dark grey boundaries
l <- list(color = toRGB("darkgray"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = TRUE,
  showcoastlines = TRUE,
  coastlinecolor = toRGB("lightgray"),
  showland = TRUE,
  landcolor = toRGB("lightgray"),
  showocean = TRUE,
  oceancolor = toRGB("white"),
  showcountries = TRUE,
  countrycolor = toRGB("darkgray"),
  projection = list(type = "Mercator")
)

fig <- plot_geo(kickstarter)
fig <- fig %>% add_trace(
  z = log10(country.freq),
  color = log10(country.freq),
  text = paste("<b>Country:</b>", names(country.freq),
               "<br><b>Projects:</b>", format(country.freq, big.mark="'"),
               "<br><b>Relative:</b>", paste0(country.rel.freq, "%")
  ),
  locations = countrycode(names(country.freq), "country.name", "iso3c"),
  marker = list(line = l, type = "log"),
  hovertemplate = paste(
    "%{text}<extra></extra>"
  )
)

country.log_max_min <- seq(floor(log10(min(country.freq))), ceiling(log10(max(country.freq))), by = 1)

fig <- fig %>% colorbar(
  title = "Amount of projects",
  tickvals = country.log_max_min,
  ticktext = 10^country.log_max_min  # Map log values to their corresponding raw counts
)

fig <- fig %>% layout(
  title = list(text = "Amount of Kickstarter projects per country", yanchor = "top"),
  geo = g
)

fig
