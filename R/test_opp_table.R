# Test opp table

library(dplyr)
library(sparkline)
library(reactable)

data <- chickwts %>%
  group_by(feed) %>%
  summarise(weight = list(weight)) %>%
  mutate(boxplot = NA, sparkline = NA)

reactable(data, columns = list(
  weight = colDef(cell = function(values) {
    sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(chickwts$weight))
  }),
  boxplot = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]], type = "box")
  }),
  sparkline = colDef(cell = function(value, index) {
    sparkline(data$weight[[index]])
  })
))