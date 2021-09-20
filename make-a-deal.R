#!/usr/bin/env Rscript

library(tidyverse)

rpareto <- function(n, shape, scale = 1) {
  scale / (runif(n) ** (1 / shape))
}

sampling_functions <- tribble(
  ~distribution, ~sample_fun,
  "linear", function(n) sample(1:n),
  "normal", rnorm,
  "pareto5", function(n) rpareto(n, 5),
  "pareto1", function(n) rpareto(n, 1),
)

simulate <- function(N, M, sample_fun) {
  x <- sample_fun(N)

  if (M == 0) {
    i <- 1
  } else if (M == N) {
    i <- N
  } else {
    threshold <- max(x[1:M])
    i <- detect_index(x, ~ . > threshold)
    if (i == 0) i <- N
  }

  x[i]
}

results <- crossing(
  N = 100,
  M = c(0, seq(0, 100, by = 5))
) %>%
  filter(M > 0) %>%
  crossing(
    distribution = sampling_functions$distribution,
    iter = 1:1000
  ) %>%
  left_join(sampling_functions, by = "distribution") %>%
  mutate(value = pmap_dbl(list(N, M, sample_fun), simulate))

results %>%
  group_by(N, M, distribution) %>%
  summarize(
    mean_value = mean(value),
    median_value = median(value),
    .groups = "drop"
  ) %>%
  pivot_longer(ends_with("value")) %>%
  ggplot(aes(M, value, color = name)) +
  facet_wrap(vars(distribution), scales = "free") +
  geom_line()
