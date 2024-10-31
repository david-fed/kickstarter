kickstarter <- read.csv("./data/kickstarter_projects.csv", encoding = "UTF-8")

library(dplyr)
library(ggplot2)
library(ggdark)

kickstarter.success.category <- kickstarter %>%
  group_by(Category) %>%
  summarize(success_rate = mean(State == "Successful", na.rm = TRUE),
            mean_goal = mean(Goal, na.rm = TRUE),
            project_count = n()
  )

ggplot(kickstarter.success.category, aes(x = reorder(Category, success_rate), y = success_rate, fill = mean_goal)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = format(project_count, big.mark = "'")), vjust = -0.5) +
  scale_fill_viridis_c(name = "Mean Goal", option = "plasma") +
  labs(title = "Success rate by category\nwith average goal amount & project amount",
       x = "Category",
       y = "Success Rate") +
  dark_theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "#1e1e1e"),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2)
  )
