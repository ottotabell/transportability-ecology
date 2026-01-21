library(MASS)
library(dplyr)
library(ggplot2)

# Assuming portland and siteinfo have already been manipulated at portland.R

sites <- portland %>% select(watershed, curbid, prism_elev_m, prism_atemp_max_C, prism_ppt_mm,
                             season)
sites <- sites %>% mutate(elevation = prism_elev_m, site_id = curbid,
                          precipitation = prism_ppt_mm, temperature = prism_atemp_max_C)

set.seed(260113)

# Generate site-level data
generate_data <- function() {
  
  
  # Standardize environmental variables for modeling
  sites$elevation_std <- scale(sites$elevation)[,1]
  sites$precip_std <- scale(sites$precipitation)[,1] 
  sites$temp_std <- scale(sites$temperature)[,1]
  
  # Calculate land use probabilities based on environmental variables
  # First, get unique site characteristics (one row per site_id)
  site_characteristics <- sites %>%
    select(site_id, elevation_std, precip_std, temp_std) %>%
    distinct(site_id, .keep_all = TRUE) %>%
    rowwise() %>%
    mutate(
      # Base probabilities that will be adjusted by environmental conditions
      prob_forest = exp(0.3 * elevation_std + 0.2 * precip_std - 0.1 * temp_std),
      prob_agriculture = exp(-0.4 * elevation_std + 0.1 * precip_std + 0.2 * temp_std),
      prob_urban = exp(-0.2 * elevation_std - 0.1 * precip_std + 0.3 * temp_std),
      prob_other = exp(0),  # Baseline probability
      
      # Normalize probabilities to sum to 1
      total_prob = prob_forest + prob_agriculture + prob_urban + prob_other,
      prob_forest = prob_forest / total_prob,
      prob_agriculture = prob_agriculture / total_prob,
      prob_urban = prob_urban / total_prob,
      prob_other = prob_other / total_prob,
      
      # Sample land use based on these probabilities (once per site)
      land_use = sample(c("forest", "agriculture", "urban", "other"), 
                        size = 1, 
                        prob = c(prob_forest, prob_agriculture, prob_urban, prob_other))
    ) %>%
    ungroup() %>%
    select(site_id, land_use)
  
  # Join the land use back to all observations for each site
  sites <- sites %>%
    left_join(site_characteristics, by = "site_id")
  
  # Now calculate canopy cover based on land use and environmental variables
  sites <- sites %>%
    group_by(site_id) %>%
    mutate(
      canopy_cover = case_when(
        land_use == "forest" ~ pmax(45, pmin(75,
                                             65 + 0.3 * precip_std + 0.2 * temp_std + rnorm(1, 0, 4))),
        land_use == "agriculture" ~ pmax(45, pmin(75,
                                                  50 + 0.2 * precip_std + 0.1 * temp_std + rnorm(1, 0, 4))),
        land_use == "urban" ~ pmax(45, pmin(75,
                                            55 + 0.1 * precip_std + rnorm(1, 0, 4))),
        TRUE ~ pmax(45, pmin(75,
                             58 + 0.25 * precip_std + 0.15 * temp_std + rnorm(1, 0, 4)))
      )
    ) %>%
    ungroup()
  
  sites <- sites %>% mutate(
    seasonal_baseline = case_when(
      season == "spring" ~ 12,
      season == "summer" ~ 18, 
      season == "fall" ~ 11,
      season == "winter" ~ 9
    ),
    water_temp = seasonal_baseline + 
      0.5 * (temperature - 11.5) +                          # Air temp effect (centered)
      -0.003 * (elevation - 500) +                          # Elevation cooling (centered)  
      -0.0008 * (precipitation - 1000) +                    # Precipitation cooling (centered)
      -0.04 * (canopy_cover - 60) * ifelse(season == "summer", 1.5, 1.0) + # Canopy shading (centered)
      rnorm(nrow(sites), 0, 1.0),
    
    dissolved_oxygen = 9.5 +                                # Baseline O2 concentration
      -0.3 * (water_temp - 12) +                           # Temperature effect (strong negative)
      0.002 * (elevation - 500) +                          # Elevation effect (positive)
      0.0005 * (precipitation - 1000) +                    # Precipitation effect (slight positive)
      -0.1 * (temperature - 11.5) +                        # Air temp effect (negative)
      case_when(                                            # Land use effects
        land_use == "urban" ~ -1.2,                        # Urban pollution/runoff
        land_use == "agriculture" ~ -0.6,                  # Agricultural runoff
        land_use == "forest" ~ 0.8,                        # Forest increases O2
        TRUE ~ 0
      ) +
      rnorm(nrow(sites), 0, 0.4)
  )
  
  return(sites)
}

full_data <- generate_data()

frontdoor_effect <- function(canopy, data_fit, data_pred = NULL) {
  if (is.null(data_pred)) data_pred <- data_fit
  p <- numeric(length(canopy))
  
  data_matrix <- cbind(data_pred$precipitation, data_pred$temperature, data_pred$elevation)
  
  mu_est <- colMeans(data_matrix)
  Sigma_est <- cov(data_matrix)
  samples <- mvrnorm(n = 10000, mu = mu_est, Sigma = Sigma_est)
  
  samples <- data.frame(samples)
  colnames(samples) = c("precipitation", "temperature", "elevation")
  
  m1 <- lm(water_temp ~ canopy_cover + precipitation + temperature + elevation, data = data_fit)
  
  m2 <- lm(canopy_cover ~ precipitation + temperature + elevation, data = data_fit)
  
  m3 <- lm(dissolved_oxygen ~ water_temp + canopy_cover + precipitation + temperature + elevation, data = data_fit)
  
  for (i in 1:length(p)) {
    samples$water_temp <- predict(m1, samples %>% mutate(canopy_cover = canopy[i]))
    samples$canopy_cover <- predict(m2, samples)
    p[i] <- predict(m3, samples) |> mean()
  }
  
  return(p)
}

d0 <- full_data[full_data$watershed != "Fanno Creek", ]
d1 <- full_data[full_data$watershed == "Fanno Creek", ]

x1_fix <- 55


# 1a: biased source population back-door adjustment 

m0 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover, data = d0)

predict(m0, d0 %>% mutate(canopy_cover = x1_fix)) |> mean()

# 1b: biased target population back-door adjustment 

m1 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover, data = d1)

predict(m1, d1 %>% mutate(canopy_cover = x1_fix)) |> mean()

# 1c: biased transportation with back-door adjustment 

m2 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover, data = d0)
predict(m2, d1 %>% mutate(canopy_cover = x1_fix)) |> mean()

# 2a: source population back-door adjustment

m3 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover + land_use, data = d0)

predict(m3, d0 %>% mutate(canopy_cover = x1_fix)) |> mean()

# 2b: target population back-door adjustment

m4 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover + land_use, data = d1)

predict(m4, d1 %>% mutate(canopy_cover = x1_fix)) |> mean()

# 2c: transportation with back-door

m5 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover + land_use, data = d0)
predict(m5, d1 %>% mutate(canopy_cover = x1_fix)) |> mean()

# 3a: source population front-door adjustment

frontdoor_effect(x1_fix, d0)

# 3b: target population front-door adjustment

frontdoor_effect(x1_fix, d1)

# 3c: transportation with front-door adjustment

frontdoor_effect(x1_fix, d0, d1)

# Single simulation run
run_single_simulation <- function(sim_id, x1_fix = 55) {
  
  full_data <- generate_data()
  
  # Split data (watershed 2 as target, others as source)
  d0 <- full_data[full_data$watershed != "Fanno Creek", ]
  d1 <- full_data[full_data$watershed == "Fanno Creek", ]  # Target population
  
  # 1a
  m0 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover, data = d0)
  est0 <- predict(m0, d0 %>% mutate(canopy_cover = x1_fix)) |> mean()
  
  # 1b
  m1 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover, data = d1)
  est1 <- predict(m1, d1 %>% mutate(canopy_cover = x1_fix)) |> mean()
  
  # 1c
  m2 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover, data = d0)
  est2 <- predict(m2, d1 %>% mutate(canopy_cover = x1_fix)) |> mean()
  
  # 2a
  m3 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover + land_use, data = d0)
  est3 <- predict(m3, d0 %>% mutate(canopy_cover = x1_fix)) |> mean()
  
  # 2b
  m4 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover + land_use, data = d1)
  est4 <- predict(m4, d1 %>% mutate(canopy_cover = x1_fix)) |> mean()
  
  # 2c
  m5 <- lm(dissolved_oxygen ~ precipitation + temperature + elevation + canopy_cover + land_use, data = d0)
  est5 <- predict(m5, d1 %>% mutate(canopy_cover = x1_fix)) |> mean()
  
  # 3a
  est6 <- tryCatch({
    frontdoor_effect(x1_fix, d0)
  }, error = function(e) NA)
  
  # 3b
  est7 <- tryCatch({
    frontdoor_effect(x1_fix, d1)
  }, error = function(e) NA)
  
  # 3c
  est8 <- tryCatch({
    frontdoor_effect(x1_fix, d0, d1)
  }, error = function(e) NA)
  
  # Return results
  data.frame(
    sim_id = sim_id,
    m0_biased_source = est0,
    m1_biased_target = est1,
    m2_biased_transport = est2,
    m3_correct_source = est3,
    m4_correct_target = est4,
    m5_correct_transport = est5,
    m6_frontdoor_source = est6,
    m7_frontdoor_target = est7,
    m8_frontdoor_transport = est8
  )
}

n_sims <- 5000 

results_list <- lapply(1:n_sims, function(i) {
  if (i %% 100 == 0) cat("Simulation", i, "of", n_sims, "\n")
  run_single_simulation(i)
})

# Combine results
results_df <- do.call(rbind, results_list)

# Calculate summary statistics
summary_stats <- results_df %>%
  select(-sim_id) %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    q25 = ~quantile(., 0.25, na.rm = TRUE),
    q75 = ~quantile(., 0.75, na.rm = TRUE)
  ))

print("Summary Statistics:")
print(summary_stats)

# Reshape data for plotting
results_long <- results_df %>%
  select(-sim_id) %>%
  tidyr::pivot_longer(everything(), names_to = "method", values_to = "estimate") %>%
  mutate(
    method = factor(method, 
                    levels = c("m0_biased_source", "m1_biased_target", "m2_biased_transport",
                               "m3_correct_source", "m4_correct_target", "m5_correct_transport",
                               "m6_frontdoor_source", "m7_frontdoor_target", "m8_frontdoor_transport"),
                    labels = c("1a: Biased\nSource", "1b: Biased\nTarget", "1c: Biased\nTransport",
                               "2a: Back-door\nSource", "2b: Back-door\nTarget", "2c: Back-door\nTransport", 
                               "3a: Two-door\nSource", "3b: Two-door\nTarget", "3c: Two-door\nTransport"))
  )

# Create boxplot
p1 <- ggplot(results_long, aes(x = method, y = estimate, fill = method)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = median(results_df$m4_correct_target, na.rm = TRUE), 
             linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Estimation Method",
    y = "Estimated Dissolved Oxygen (mg/L)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none",
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  scale_fill_brewer(type = "qual", palette = "Set3")

print(p1)
