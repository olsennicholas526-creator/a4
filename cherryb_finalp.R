library(tidyverse)
library(mgcv)
library(stringr)
setwd("C:/Users/olsen/Downloads/peak-bloom-prediction-main/peak-bloom-prediction-main")

cherry <- read.csv("data/washingtondc.csv") |>
  bind_rows(read.csv("data/liestal.csv")) |>
  bind_rows(read.csv("data/kyoto.csv")) |>
  bind_rows(read.csv("data/vancouver.csv")) |>
  bind_rows(read.csv("data/nyc.csv")) |>
  mutate(location = factor(location))

gam_fit <- gam(
  bloom_doy ~ location + s(year, k = 10),
  data = cherry |> filter(year >= 1880),
  method = "REML"
)

pred_grid <- expand_grid(
  location = levels(cherry$location),
  year = 2026
) |>
  mutate(location = factor(location, levels = levels(cherry$location)))

p <- predict(gam_fit, newdata = pred_grid, se.fit = TRUE)

pred_2026 <- pred_grid |>
  mutate(
    doy = round(p$fit),
    date = as.Date(doy - 1, origin = "2026-01-01")
  ) |>
  select(location, doy, date)

pred_2026




