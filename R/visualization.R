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

plot_mixed_model_effects <- function(fit){

  if(!("lmerMod" %in% class(fit))) {
    stop("The input model must be of class 'lmerMod' (from the lme4 package).")
  }

  mu <- fixef(fit)["trt"]
  tau2 <- VarCorr(fit)$trial["trt", "trt"]
  var_mu <- vcov(fit)["trt", "trt"]
  df_t <- summary(fit)$ngrps - 2

  ci_mu <- confint(fit_random_hte, method = "Wald")["trt", ]

  # t critical value for 95% PI
  t_crit <- qt(0.975, df = df_t)

  # 95% Prediction Interval
  pred_lower <- mu - t_crit * sqrt(tau2 + var_mu)
  pred_upper <- mu + t_crit * sqrt(tau2 + var_mu)

  caption_text <- sprintf(
    "Pooled treatment effect: %.2f [%.2f, %.2f]; 95%% prediction interval: [%.2f, %.2f]; tau² = %.3f",
    mu, ci_mu[1], ci_mu[2], pred_lower, pred_upper, tau2
  )

  coefs <- coef(fit)$trial


  # Assume `coefs` is the result of coef(fit_random_hte)$trial
  coefs_df <- coefs %>%
    tibble::rownames_to_column("trial")

  ci_fixed <- confint(fit, level = 0.95, method = "Wald") %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    filter(grepl("^trialTrial_", term)) %>%
    mutate(trial = gsub("trialTrial_", "Trial_", term)) %>%
    rename(ci_lower = `2.5 %`, ci_upper = `97.5 %`) %>%
    select(trial, ci_lower, ci_upper)

  # Extract intercepts (same for all rows → just take the first row)
  intercepts <- coefs_df[1, paste0("trialTrial_", 1:5)] %>%
    pivot_longer(everything(), names_to = "trial_label", values_to = "estimate") %>%
    mutate(
      trial = gsub("trialTrial_", "Trial_", trial_label),
      parameter = "intercept"
    ) %>%
    left_join(ci_fixed, by = "trial") %>%
    select(trial, parameter, estimate, ci_lower, ci_upper)

  # Extract trial-specific treatment effects
  slopes <- coefs_df %>%
    select(trial, trt) %>%
    mutate(parameter = "treatment", estimate = trt) %>%
    select(trial, parameter, estimate)

  # Combine for plotting
  plot_df <- bind_rows(intercepts, slopes) %>% mutate (shape = "Circle")

  # Add summary
  plot_df <- plot_df %>% add_row(data.frame(trial="Pooled", parameter="treatment",
                                            estimate=mu,
                                            ci_lower=ci_mu[1], ci_upper=ci_mu[2]),
                                 shape = "Diamond") %>%
    mutate(parameter = case_when(
      parameter == "treatment" ~ "Treatment Effect",
      parameter == "intercept" ~ "Intercept"
    ))

  # Plot forest with facets
  ggplot(plot_df, aes(x = estimate, y = trial, shape = shape)) +
    geom_point(size = 3) +
    scale_shape_manual(values = c("Circle" = 16, "Diamond" = 18)) +
    guides(shape = "none") +

    # Prediction interval for "Pooled" only
    geom_errorbarh(
      data = data.frame(
        trial = "Pooled",
        parameter = "Treatment Effect",
        estimate = mu,
        pred_lower = pred_lower,
        pred_upper = pred_upper,
        shape = "Diamond"
      ),
      aes(xmin = pred_lower, xmax = pred_upper),
      height = 0.4,
      color = "darkred",
      linetype = "dashed"
    ) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, na.rm = TRUE) +

    facet_wrap(~parameter, scales = "free_x") +
    labs(
      caption = caption_text,
      x = "Estimate",
      y = NULL
    )  +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 8, hjust = 0)
    )
}
