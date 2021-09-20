#!/usr/bin/env Rscript

library(tidyverse)

rpareto <- function(n, shape, scale = 1) {
  scale / (runif(n) ** (1 / shape))
}

sampling_functions <- tribble(
  ~distribution, ~sample_fun,
  "uniform", runif,
  "normal", rnorm,
  "pareto1", function(n) rpareto(n, 1),
  "pareto2", function(n) rpareto(n, 2),
  "pareto3", function(n) rpareto(n, 3)
)

results <- crossing(
  M = c(1, 5, 10, 25, 50, 75, 100, 200, 500, 1000),
  distribution = sampling_functions$distribution,
  iter = 1:1e3
) %>%
  left_join(sampling_functions, by = "distribution") %>%
  mutate(max_x = map2_dbl(sample_fun, M, ~ max(do.call(.x, list(.y)))))

summary <- results %>%
  group_by(M, distribution) %>%
  summarize(
    mean_max_x = mean(max_x),
    median_max_x = median(max_x),
    .groups = "drop"
  ) %>%
  pivot_longer(ends_with("max_x"))

summary %>%
  ggplot(aes(M, value, color = name)) +
  facet_wrap(vars(distribution), scales = "free") +
  geom_line()

summary %>%
  group_by(distribution, name) %>%
  mutate(
    relative_to_10 = value / value[M == 10],
    relative_to_100 = value / value[M == 100]
  ) %>%
  ungroup() %>%
  select(M, distribution, metric = name, starts_with("relative_to")) %>%
  pivot_longer(starts_with("relative_to"), names_to = "relative_to") %>%
  ggplot(aes(M, value, color = metric, linetype = relative_to)) +
  facet_wrap(vars(distribution), scales = "free") +
  geom_line()
