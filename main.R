library(tidyverse)
library(janitor)
library(scales)
library(rstanarm)
library(gtsummary)
library(ggpmisc)

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
  geom_smooth(se=FALSE) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title="Profits of Different Ski Resorts")

temp = data |>
  filter(Resort == "Alpine Arena") |>
  lm(formula = Year ~ Profit)

summary(temp)

data |>
  filter(Resort == "Mountain Meadows") |>
  ggplot(aes(x = Year, y = Profit)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title="Projection of Mountain Meadows") +
  theme_bw()

data |>
  filter(Resort == "Alpine Arena") |>
  ggplot(aes(x = Year, y = Profit)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title="Projection of Alpine Arena") +
  theme_bw()

data |>
  filter(Resort == "White Haven") |>
  ggplot(aes(x = Year, y = Profit)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title="Projection of White Haven") +
  theme_bw()

mm = data |>
  filter(Resort == "Mountain Meadows")

fitted = stan_glm(formula = Profit ~ Year,
                    data = mm,
                    refresh = 0,
                    seed = 9)
newobs = tibble(year=2050)

temp = posterior_epred(fitted) |>
  as_tibble()
