library(tidyverse)

data <- read_csv("Ski-Resort-Dataset.csv")

data <- data |>
  rename(Profit = `Profit ($ thousands)`) |>
  mutate(Snowfall = ifelse(Snowfall == "Typpical", "typical", Snowfall)) |>
  mutate(Profit = Profit * 1000)

# snowfall and profit
data |>
  ggplot(aes(x = Snowfall, y=Profit)) +
  geom_point() +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title="Snowfall and Profit",
       subtitle= "Notice that there is one outlier in the heavy snowfall category\nthat didn't get as much profit")

# resort and profit
data |>
  ggplot(aes(x = Year, y=Profit, color=Resort)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title="Profits of Different Ski Resorts")
