#!/usr/bin/env Rscript

library(tidyverse)

rpareto <- function(n, shape, scale = 1) {
  scale / (runif(n) ** (1 / shape))
}

sampling_functions <- tribble(
  ~distribution, ~sample_fun,
  "uniform", runif,
  "normal", rnorm,
  "pareto1.16", function(n) rpareto(n, 1.16),
  "pareto2", function(n) rpareto(n, 2),
  "pareto5", function(n) rpareto(n, 5)
)

results <- crossing(
  N = c(1:100, 200, 500, 1000),
  distribution = sampling_functions$distribution,
  iter = 1:1e3
) %>%
  left_join(sampling_functions, by = "distribution") %>%
  mutate(max_x = map2_dbl(sample_fun, N, ~ max(do.call(.x, list(.y)))))

summary <- results %>%
  group_by(N, distribution) %>%
  summarize(
    mean_max_x = mean(max_x),
    median_max_x = median(max_x),
    .groups = "drop"
  ) %>%
  pivot_longer(ends_with("max_x"))

max_reward <- function(f, x0) {
  obj <- function(x) f(x) - f(x0) / x0 * x
  optimize(obj, c(0, x0), maximum = TRUE)$maximum
}

curves <- summary %>%
  nest(data = c(N, value)) %>%
  mutate(fun = map(data, ~ approxfun(.$N, .$value))) %>%
  crossing(x0 = c(10, 100, 1000)) %>%
  mutate(
    fit = map2_dbl(fun, x0, max_reward)
  )

curves %>%
  select(distribution, name, x0, fit) %>%
  pivot_wider(names_from = x0, values_from = fit) %>%
  arrange(name) %>%
  print(n = Inf)

summary %>%
  ggplot(aes(N, value, color = name)) +
  facet_wrap(vars(distribution), scales = "free") +
  geom_line()
