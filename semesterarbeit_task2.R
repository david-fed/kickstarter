kickstarter <- read.csv("./data/kickstarter_projects.csv", encoding = "UTF-8")

library(plotly)

set.seed(123)

kickstarter.filtered <- kickstarter[kickstarter$State %in% c("Successful", "Failed"), ]
kickstarter.filtered.sampled <- kickstarter[sample(nrow(kickstarter.filtered), 30000), ]

plot_ly(data = kickstarter.filtered.sampled, x = ~Goal, y = ~Pledged,
        type = "scatter", mode = "markers",
        marker = list(color = "rgba(0, 0, 0, 0.1)", size = 6),
        text = ~paste(
          "Name:", Name,
          "<br>Category:", Category,
          "<br>Subcategory:", Subcategory,
          "<br>State:", State,
          "<br>Backers:", format(Backers, big.mark="'", trim = TRUE),
          "<br>Goal Amount:", format(Goal, big.mark="'", trim = TRUE),
          "<br>Pledged Amount:", format(Pledged, big.mark="'", trim = TRUE)
        ),
        hoverinfo = "text") %>%
  layout(
    title = "Goal vs Pledged Amounts",
    xaxis = list(title = "Goal Amount (Log Scale)", type = "log"),
    yaxis = list(title = "Pledged Amount (Log Scale)", type = "log"),
    margin = list(t = 60, b = 60),
    shapes = list(
      type = "line",
      x0 = 1,
      x1 = max(kickstarter.filtered.sampled$Pledged),
      y0 = 1,
      y1 = max(kickstarter.filtered.sampled$Pledged),
      line = list(dash="dash", width=2, color = "red"),
      layer = "below"
    )
  )
