# 1. Plot settings for ggplot
theme_set(
  theme_bw() +
    theme(
      text = element_text(size = 18, face = "bold"), 
      title = element_text(size = 18, face = "bold"),
      legend.position = "bottom"
    )
)


# 2. Multicore settings
options(
  mc.cores = parallel::detectCores(),
  future.fork.enable = TRUE,
  future.rng.onMisuse = "ignore"
)