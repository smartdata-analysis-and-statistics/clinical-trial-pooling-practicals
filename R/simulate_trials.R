simulate_trials <- function(
    baseline_means,
    n_per_arm = 50,
    baseline_sd_fn = function(mean) 2 + 0.2 * mean,
    change_sd = 5,
    cfb_active,
    cfb_control,
    trial_intercepts = NULL,
    beta_W = NULL, # within-trial effect modification
    beta_A = 0,   # across-trial effect modification
    seed = 2025,
    add_aux_vars = FALSE
) {
  set.seed(seed)

  n_trials <- length(baseline_means)

  # Default: no shift
  if (is.null(trial_intercepts)) {
    trial_intercepts <- rep(0, n_trials)
  }

  if (is.null(beta_W)) {
    beta_W <- rep(0, n_trials)
  }

  data_list <- lapply(seq_len(n_trials), function(i) {
    # Step 1: trial-specific mean baseline (X̄_k), around overall mean
    xk_mean <- baseline_means[i]

    # Step 2: trial-specific baseline SD
    xk_sd <- baseline_sd_fn(xk_mean)

    # Step 3: simulate treatment (1 = active, 0 = control)
    trt <- rep(c(1, 0), each = n_per_arm)

    # Step 4: simulate baseline
    baseline <- c(
      rnorm(n_per_arm, mean = xk_mean, sd = xk_sd),  # active
      rnorm(n_per_arm, mean = xk_mean, sd = xk_sd)   # control
    )

    cbase <- (baseline - xk_mean)

    # Step 5: derive change from baseline due to treatment
    cfb <- (1-trt) * cfb_control[i]  +  trt * (cfb_active[i])

    # Step 6: derive within-study interaction effect
    cfb_tx <- trt * beta_W[i] * cbase


    # Step 7: compute change using within-trial and across-trial terms
    error <- rnorm(2 * n_per_arm, mean = 0, sd = change_sd)
    outcome <-  trial_intercepts[i] +
      baseline +
      cfb +
      cfb_tx + # Add within-study interaction
      beta_A * xk_mean +
      error

    data.frame(
      trial = paste0("Trial_", i),
      trt = trt,
      trta = ifelse(trt == 1, "Active", "Control"),
      intercept = trial_intercepts[i],
      baseline = baseline,
      cbase = cbase,
      cfb = cfb,
      cfb_tx = cfb_tx,
      error = error,
      outcome = outcome,
      change = outcome-baseline,
      xk_mean = xk_mean
    )
  })

  out <- do.call(rbind, data_list)
  out <- out %>% mutate(trta = factor(ifelse(trt == 1, "Active", "Control"),
                                      levels = c("Control", "Active")))

  if(!add_aux_vars) {
    out <- out %>% select(-c(error, intercept,
                             cfb, cfb_tx))
  }

  return(out)
}
