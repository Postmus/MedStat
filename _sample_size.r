## Example: library vs coffee bar -----------------------------------------

# group sizes
n1 <- 40  # library
n2 <- 40  # coffee bar

# means and SDs (fixed, not simulated)
mean1 <- 67  # library
mean2 <- 69  # coffee bar

sd1 <- 10
sd2 <- 10

# test statistic (library - coffee bar)
se    <- sqrt(sd1^2 / n1 + sd2^2 / n2)
t_obs <- (mean1 - mean2) / se
df    <- n1 + n2 - 2

t_obs
df

# p-values ---------------------------------------------------------------

# two-sided p-value
p_two <- 2 * pt(abs(t_obs), df = df, lower.tail = FALSE)

# correct one-sided p-values
# alternative: library < coffee bar
p_one_less    <- pt(t_obs, df = df)         

# alternative: library > coffee bar
p_one_greater <- 1 - pt(t_obs, df = df)     

# SPSS-style (incorrect if direction does not match sign)
p_one_spss <- p_two / 2

p_two
p_one_less
p_one_greater
p_one_spss


## Plotting functions -----------------------------------------------------

library(ggplot2)

make_t_df <- function(t_obs, df, type = c("two.sided", "less", "greater")) {
  type <- match.arg(type)

  x_vals <- seq(-4, 4, length.out = 2000)
  dens   <- dt(x_vals, df = df)

  if (type == "two.sided") {
    # shade both tails beyond ±|t|
    crit  <- abs(t_obs)
    shade <- (x_vals <= -crit) | (x_vals >= crit)
  } else if (type == "less") {
    # shade the lower tail
    shade <- x_vals <= t_obs
  } else {  # "greater"
    # shade the upper tail
    shade <- x_vals >= t_obs
  }

  data.frame(
    x = x_vals,
    dens = dens,
    shade = shade
  )
}

plot_t_with_p <- function(t_obs, df, type = c("two.sided", "less", "greater"),
                          title = "t-distribution") {
  type    <- match.arg(type)
  df_plot <- make_t_df(t_obs, df, type)

  ggplot(df_plot, aes(x = x, y = dens)) +
    geom_line() +
    geom_area(
      aes(x = x, y = ifelse(shade, dens, 0)),
      alpha = 0.4
    ) +
    geom_vline(xintercept = t_obs, linetype = "dashed") +
    labs(x = "t", y = "Density", title = title) +
    theme_minimal()
}

## Generate the three plots ----------------------------------------------

plot_two_sided <- plot_t_with_p(
  t_obs, df, "two.sided",
  title = "Two-sided p-value (difference in either direction)"
)

plot_one_less <- plot_t_with_p(
  t_obs, df, "less",
  title = "One-sided p-value (Library < Coffee bar)"
)

plot_one_greater <- plot_t_with_p(
  t_obs, df, "greater",
  title = "One-sided p-value (Library > Coffee bar)"
)

plot_two_sided
plot_one_less
plot_one_greater
