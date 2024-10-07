kickstarter <- read.csv("./data/kickstarter_projects.csv", encoding = "UTF-8")
## Grafik 1 ----
category.relative.frequency <- table(kickstarter$Category) / length(kickstarter$Category) * 100
par(mar=c(8, 6, 4, 2))
balken <- barplot(
  sort(category.relative.frequency, decreasing = TRUE),
  las = 2,
  main = "Kategoriehauefigkeit von Kickstarter Projekte",
  col = "#05ce78",
  ylim = c(0, 20),
  border = "white"
)
title(xlab="Kategorie", line=6)
title(ylab="Relative Haeufigkeit (%)", line=3)
text(
  x = balken,
  y = sort(category.relative.frequency, decreasing = TRUE) + 0.5,
  labels = round(
    sort(category.relative.frequency,
         decreasing = TRUE
    ),
    1)
)
## Grafik 2 ----
backers <- kickstarter$Backers
hist(log(backers))