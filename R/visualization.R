library(ggplot2)

plot_change_by_treatment_with_means <- function(df, title = NULL,
                                                subtitle = NULL) {
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
      title = title,
      subtitle = subtitle,
      x = "Baseline Severity",
      y = "Change from Baseline"
    ) +
    facet_wrap(~ trta, ncol = 1) +
    theme_minimal() +
    facet_wrap(~ trta, ncol = 1) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


plot_outcome_predictions_by_treatment <- function(model) {
  df <- model$data

  # Prepare new data for prediction
  new_data_strat <- expand.grid(
    baseline = seq(min(df$baseline), max(df$baseline), length.out = 100),
    trta = levels(df$trta),
    trial = unique(df$trial)
  )

  # Get model predictions
  new_data_strat$pred <- predict(fit_stratified, newdata = new_data_strat)

  # Plot: Observed outcomes and model-predicted lines by treatment
  ggplot() +
    # Model prediction lines (trial-stratified intercepts, shared slope & treatment effect)
    geom_line(data = new_data_strat, aes(x = baseline, y = pred, group = trial, color = trial), linewidth = 1.2) +

    # Observed data points colored by trial
    geom_point(data = df, aes(x = baseline, y = outcome, color = trial), alpha = 0.5, size = 1.5) +

    # Horizontal reference line
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +

    # Facet by treatment group
    facet_wrap(~trta) +

    # Labels
    labs(
      title = "Observed Outcomes and Model-Predicted Values by Treatment Group",
      x = "Baseline Severity",
      y = "Observed Outcome"
    ) +

    # Theme
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0, size = 8, face = "italic"),
      legend.position = "bottom"
    )
}
