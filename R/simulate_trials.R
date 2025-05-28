simulate_trials <- function(
    baseline_means,              # Vector of baseline means (length = # trials)
    n_per_arm = 50,
    baseline_sd_fn = function(mean) 2 + 0.2 * mean,
    change_sd = 5,
    cfb_active,
    cfb_control,
    trial_intercepts = NULL,
    beta_W = 0, # within-trial effect modification
    beta_A = 0,   # across-trial effect modification
    seed = 2025
) {
  set.seed(seed)

  n_trials <- length(baseline_means)

  # Default: no shift
  if (is.null(trial_intercepts)) {
    trial_intercepts <- rep(0, n_trials)
  }

  data_list <- lapply(seq_len(n_trials), function(i) {
    # Step 1: trial-specific mean baseline (X̄_k), around overall mean
    xk_mean <- baseline_means[i]

    # Step 2: trial-specific baseline SD
    xk_sd <- baseline_sd_fn(xk_mean)

    # Step 3: trial-specific treatment effect (α + βA * X̄_k)
    treatment_effect <- (cfb_active[i] - cfb_control[i]) +
      beta_A * xk_mean  # ecological effect modifies difference

    # Step 4: simulate treatment (1 = active, 0 = control)
    trt <- rep(c(1, 0), each = n_per_arm)

    # Step 5: simulate baseline (X_ik)
    baseline <- c(
      rnorm(n_per_arm, mean = xk_mean, sd = xk_sd),  # active
      rnorm(n_per_arm, mean = xk_mean, sd = xk_sd)   # control
    )

    # Step 6: simulate centered covariate (X_ik - X̄_k)
    x_centered <- baseline - xk_mean

    # Step 7: compute change using within-trial and across-trial terms
    error <- rnorm(2 * n_per_arm, mean = 0, sd = change_sd)
    outcome <-  trial_intercepts[i] + baseline + cfb_control[i] + trt*treatment_effect + beta_W * x_centered + error

    data.frame(
      trial = paste0("Trial_", i),
      trt = trt,
      trta = ifelse(trt == 1, "Active", "Control"),
      baseline = baseline,
      change = outcome-baseline,
      outcome = outcome,
      x_centered = x_centered,
      xk_mean = xk_mean
    )
  })

  out <- do.call(rbind, data_list)
  out <- out %>% mutate(trta = factor(ifelse(trt == 1, "Active", "Control"), levels = c("Control", "Active")))

  return(out)
}
