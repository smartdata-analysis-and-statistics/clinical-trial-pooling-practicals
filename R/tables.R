summarize_baseline_by_trial <- function(df) {

  library(dplyr)
  library(tidyr)
  library(kableExtra)

  df %>%
    group_by(trial, trta) %>%
    summarise(
      mean = mean(baseline, na.rm = TRUE),
      sd = sd(baseline, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(baseline_label = sprintf("%.1f (%.1f)", mean, sd)) %>%
    select(trial, trta, baseline_label) %>%
    pivot_wider(
      names_from = trta,
      values_from = baseline_label,
      names_prefix = "baseline_"
    ) %>%
    left_join(
      df %>%
        group_by(trial) %>%
        summarise(
          N = n(),
          mean = mean(baseline, na.rm = TRUE),
          sd = sd(baseline, na.rm = TRUE),
          baseline_overall = sprintf("%.1f (%.1f)", mean, sd),
          .groups = "drop"
        ) %>%
        select(trial, N, baseline_overall),
      by = "trial"
    ) %>%
    select(trial, N, baseline_Active, baseline_Control, baseline_overall)%>%
    kable(format = "html", escape = FALSE,
          col.names = c(
            "Trial",
            "N",
            "Baseline (Active)\nMean (SD)",
            "Baseline (Control)\nMean (SD)",
            "Baseline (Overall)\nMean (SD)"
          )) %>%
    kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover"))

}
