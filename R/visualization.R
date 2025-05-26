library(ggplot2)

plot_change_by_treatment_with_means <- function(df) {
  # Precompute mean labels
  mean_labels <- df %>%
    group_by(trta) %>%
    summarise(mean_change = mean(change, na.rm = TRUE)) %>%
    mutate(label = paste0("Mean change: ", round(mean_change, 1)))

  ggplot(df, aes(x = baseline, y = change)) +
    geom_point(aes(color=trial),alpha = 0.6) +
    #geom_smooth(method = "lm", aes(group = 1), color = "black") +
    geom_text(
      data = mean_labels,
      aes(x = -Inf, y = Inf, label = label),
      hjust = -0.1, vjust = 1.2,
      inherit.aes = FALSE
    ) +
    labs(
      title = "Change from Baseline by Treatment Group",
      subtitle = "No within- or across-trial effect modification",
      x = "Baseline Severity",
      y = "Change from Baseline"
    ) +
    facet_wrap(~ trta, ncol = 1) +
    theme_minimal() +
    facet_wrap(~ trta, ncol = 1) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
