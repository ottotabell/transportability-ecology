library(MASS)
library(tidyverse)

# Function for estimating the causal effect with back-door criterion

backdoor_effect <- function(canopy, data_fit, data_pred = NULL) {
  if (is.null(data_pred)) data_pred <- data_fit
  m <- lm(do_mgl ~ season * nlcd_2021_ttc_overwater_. + prism_elev_m + 
            prism_atemp_max_C + prism_ppt_mm, data = data_fit)
  p <- numeric(length(canopy))
  for (i in 1:length(p)) {
    p[i] <- predict(m, data_pred %>% mutate(nlcd_2021_ttc_overwater_. = canopy[i])) |> median()
  }
  return(p)
}

# Function for estimating the causal effect with two-door formula

frontdoor_effect <- function(canopy, data_fit, data_pred = NULL) {
  if (is.null(data_pred)) data_pred <- data_fit
  p <- numeric(length(canopy))
  
  data_matrix <- cbind(data_pred$prism_atemp_max_C, data_pred$prism_elev_m, data_pred$prism_ppt_mm)

  mu_est <- colMeans(data_matrix)
  Sigma_est <- cov(data_matrix)
  samples <- mvrnorm(n = nrow(data_pred), mu = mu_est, Sigma = Sigma_est)
  
  samples <- data.frame(samples)
  colnames(samples) = c("prism_atemp_max_C", "prism_elev_m", "prism_ppt_mm")
  samples$season <- data_pred$season
  
  m1 <- gam(temp_c ~ nlcd_2021_ttc_overwater_. + prism_elev_m + 
              prism_atemp_max_C + prism_ppt_mm, data = data_fit)
  
  m2 <- gam(nlcd_2021_ttc_overwater_. ~ prism_elev_m + prism_atemp_max_C + prism_ppt_mm, data = data_fit)
  
  m3 <- gam(do_mgl ~ temp_c + nlcd_2021_ttc_overwater_. + prism_elev_m + 
              prism_atemp_max_C + prism_ppt_mm + season, data = data_fit)
  
  for (i in 1:length(p)) {
    samples$temp_c <- predict(m1, samples %>% mutate(nlcd_2021_ttc_overwater_. = canopy[i]))
    samples$nlcd_2021_ttc_overwater_. <- predict(m2, samples)
    p[i] <- predict(m3, samples) |> median()
  }
  
  return(p)
}

# Bootstrapping with back-door

boot_backdoor <- function(B, canopy, data_fit, data_pred = NULL) {
  boot <- matrix(nrow = B, ncol = length(canopy))
  for (i in 1:B) {
    # Progress update every 100 iterations
    if (i %% 100 == 0) {
      cat("Bootstrap iteration:", i, "of", B, as.character(Sys.time()), "\n")
    }
    boot_fit <- data_fit[sample(1:nrow(data_fit), nrow(data_fit), replace = TRUE), ]
    boot_pred <- NULL
    if (!is.null(data_pred)) boot_pred <- data_pred[sample(1:nrow(data_pred), nrow(data_pred), replace = TRUE), ]
    boot[i, ] <- backdoor_effect(canopy, boot_fit, boot_pred)
  }
  return(boot)
}

# Bootstrapping with front-door

boot_frontdoor <- function(B, canopy, data_fit, data_pred = NULL) {
  boot <- matrix(nrow = B, ncol = length(canopy))
  for (i in 1:B) {
    # Progress update every 100 iterations
    if (i %% 100 == 0) {
      cat("Bootstrap iteration:", i, "of", B, as.character(Sys.time()), "\n")
    }
    boot_fit <- data_fit[sample(1:nrow(data_fit), nrow(data_fit), replace = TRUE), ]
    boot_pred <- NULL
    if (!is.null(data_pred)) boot_pred <- data_pred[sample(1:nrow(data_pred), nrow(data_pred), replace = TRUE), ]
    boot[i, ] <- frontdoor_effect(canopy, boot_fit, boot_pred)
  }
  return(boot)
}

calculate_ci <- function(boot_matrix, canopy) {
  mean_vals <- colMeans(boot_matrix)
  lower_ci <- apply(boot_matrix, 2, quantile, probs = 0.025)
  upper_ci <- apply(boot_matrix, 2, quantile, probs = 0.975)
  return(list(mean = mean_vals, lower = lower_ci, upper = upper_ci))
}

# Plotting the results

plot_bootstrap_comparison <- function(df_list, canopy_values) {
  # Validate inputs
  if (length(df_list) != 3) {
    stop("df_list must contain exactly 3 data frames")
  }
  if (length(canopy_values) != 4) {
    stop("canopy_values must be a vector of length 4")
  }
  
  # Convert to data frames and assign bootstrap labels
  df_list[[1]]$bootstrap <- "Source pop."
  df_list[[2]]$bootstrap <- "Transported"
  df_list[[3]]$bootstrap <- "Target pop."
  
  # Combine all data frames
  all_boots <- bind_rows(df_list[[1]], df_list[[2]], df_list[[3]])
  
  # Reshape to long format
  long_boots <- all_boots %>%
    pivot_longer(cols = -bootstrap, names_to = "x", values_to = "estimate")
  
  # Create facet labels dynamically based on canopy_values
  facet_labels <- setNames(
    paste0(1:4, ": Canopy cover fixed to ", canopy_values, "%"),
    paste0("V", 1:4)
  )
  
  # Calculate y-axis limits based on actual boxplot whisker endpoints
  # For each group, find the min/max values that would be shown (excluding outliers)
  whisker_limits <- long_boots %>%
    group_by(bootstrap, x) %>%
    summarise(
      q1 = quantile(estimate, 0.25, na.rm = TRUE),
      q3 = quantile(estimate, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      lower_whisker = max(min(estimate, na.rm = TRUE), q1 - 1.5 * iqr),
      upper_whisker = min(max(estimate, na.rm = TRUE), q3 + 1.5 * iqr),
      .groups = "drop"
    )
  
  y_min <- min(whisker_limits$lower_whisker, na.rm = TRUE)
  y_max <- max(whisker_limits$upper_whisker, na.rm = TRUE)
  
  # Add a small margin (2% of range) for visual breathing room
  y_range <- y_max - y_min
  y_min <- y_min - 0.02 * y_range
  y_max <- y_max + 0.02 * y_range
  
  # Create the plot
  ggplot(long_boots, aes(x = bootstrap, y = estimate, fill = bootstrap)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    facet_wrap(~ x, ncol = 2, labeller = as_labeller(facet_labels)) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "gray90"),
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 13)
    ) +
    labs(
      x = "Method",
      y = "Estimated Dissolved Oxygen (mg/L)"
    )
}
