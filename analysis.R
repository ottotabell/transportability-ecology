source("funcs_libs.R")

####################################
# Reading and visualising the data #
####################################

portland <- read.csv("data/portland.csv")
site <- read.csv("data/siteinfo.csv")
portland <- merge(portland, site, by = "curbid")
portland <- portland[portland$watershed != "Columbia Slough", ]
# Removing duplicate spring measurements from some sites
portland <- portland[!(portland$synoptic_event == "4a" & portland$watershed == "Willamette"), ]
# Removing a site with no tree canopy cover
portland <- portland[portland$curbid != 4084, ]
portland <- portland[portland$do_pctsat < 180, ]

# Dividing the data to Fanno Creek and the rest.

fanno <- portland %>% filter(watershed == "Fanno Creek")
rest <- portland %>% filter(watershed !=  "Fanno Creek")

###############
# Scatterplot #
###############

ggplot(portland, aes(x = nlcd_2021_ttc_overwater_., y = do_mgl, color = watershed)) +
  geom_point(alpha = 0.6, shape = 21, size = 2, stroke = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.75) +
  labs(x = "Tree Canopy Cover (%)",
       y = "Dissolved Oxygen (mg/L)",
       color = "Watershed") +
  scale_color_manual(
    values = c(
      "Clackamas" = "#666666",
      "Columbia Slough" = "cyan4",
      "Fanno Creek" = "red",
      "Johnson Creek" = "darkgreen",
      "Kellogg Creek" = "purple",
      "Lake Oswego" = "yellow",
      "Tryon Creek" = "cyan",
      "Tulatin" = "brown",
      "Willamette" = "orange"
    )
  ) +
#  facet_wrap(~ season) +  # This adds 4 separate panels, one for each season
  theme_minimal()

##########################
# Fitting and predicting #
##########################

set.seed(260113)

fanno_canopy <- c(45, 50, 55, 60)

fanno_bd_source <- boot_backdoor(2500, canopy = fanno_canopy, 
                                 portland %>% filter(watershed != "Fanno Creek"))
fanno_bd_trans <- boot_backdoor(2500, canopy = fanno_canopy, 
                                portland %>% filter(watershed != "Fanno Creek"), 
                                portland %>% filter(watershed == "Fanno Creek"))
fanno_bd_target <- boot_backdoor(2500, canopy = fanno_canopy, 
                                 portland %>% filter(watershed == "Fanno Creek"))

fanno_fd_source <- boot_frontdoor(2500, canopy = fanno_canopy, 
                                 portland %>% filter(watershed != "Fanno Creek"))
fanno_fd_trans <- boot_frontdoor(2500, canopy = fanno_canopy, 
                                portland %>% filter(watershed != "Fanno Creek"), 
                                portland %>% filter(watershed == "Fanno Creek"))
fanno_fd_target <- boot_frontdoor(2500, canopy = fanno_canopy, 
                                 portland %>% filter(watershed == "Fanno Creek"))

plot_bootstrap_comparison(list(
  as.data.frame(fanno_bd_source),
  as.data.frame(fanno_bd_trans),
  as.data.frame(fanno_bd_target)
), fanno_canopy)

plot_bootstrap_comparison(list(
  as.data.frame(fanno_fd_source),
  as.data.frame(fanno_fd_trans),
  as.data.frame(fanno_fd_target)
), fanno_canopy)


############
# Johnson Creek
############

johnson_canopy <- c(45, 50, 55, 60)

johnson_bd_source <- boot_backdoor(2500, canopy = johnson_canopy,
                                 portland %>% filter(watershed != "Johnson Creek"))
johnson_bd_trans <- boot_backdoor(2500, canopy = johnson_canopy,
                                portland %>% filter(watershed != "Johnson Creek"),
                                portland %>% filter(watershed == "Johnson Creek"))
johnson_bd_target <- boot_backdoor(2500, canopy = johnson_canopy,
                                 portland %>% filter(watershed == "Johnson Creek"))

johnson_fd_source <- boot_frontdoor(2500, canopy = johnson_canopy,
                                  portland %>% filter(watershed != "Johnson Creek"))
johnson_fd_trans <- boot_frontdoor(2500, canopy = johnson_canopy,
                                 portland %>% filter(watershed != "Johnson Creek"),
                                 portland %>% filter(watershed == "Johnson Creek"))
johnson_fd_target <- boot_frontdoor(2500, canopy = johnson_canopy,
                                  portland %>% filter(watershed == "Johnson Creek"))

plot_bootstrap_comparison(list(
  as.data.frame(johnson_bd_source),
  as.data.frame(johnson_bd_trans),
  as.data.frame(johnson_bd_target)
), c(45, 50, 55, 60))

plot_bootstrap_comparison(list(
  as.data.frame(johnson_fd_source),
  as.data.frame(johnson_fd_trans),
  as.data.frame(johnson_fd_target)
), c(45, 50, 55, 60))


############
# Willamette
############

willamette_canopy <- c(57, 58, 59, 60)

willamette_bd_source <- boot_backdoor(2500, canopy = willamette_canopy, 
                                   portland %>% filter(watershed != "Willamette"))
willamette_bd_trans <- boot_backdoor(2500, canopy = willamette_canopy, 
                                  portland %>% filter(watershed != "Willamette"), 
                                  portland %>% filter(watershed == "Willamette"))
willamette_bd_target <- boot_backdoor(2500, canopy = willamette_canopy, 
                                   portland %>% filter(watershed == "Willamette"))

willamette_fd_source <- boot_frontdoor(2500, canopy = willamette_canopy, 
                                    portland %>% filter(watershed != "Willamette"))
willamette_fd_trans <- boot_frontdoor(2500, canopy = willamette_canopy, 
                                   portland %>% filter(watershed != "Willamette"), 
                                   portland %>% filter(watershed == "Willamette"))
willamette_fd_target <- boot_frontdoor(2500, canopy = willamette_canopy, 
                                    portland %>% filter(watershed == "Willamette"))

plot_bootstrap_comparison(list(
  as.data.frame(willamette_bd_source),
  as.data.frame(willamette_bd_trans),
  as.data.frame(willamette_bd_target)
), willamette_canopy)

plot_bootstrap_comparison(list(
  as.data.frame(willamette_fd_source),
  as.data.frame(willamette_fd_trans),
  as.data.frame(willamette_fd_target)
), willamette_canopy)

############
# Tryon Creek
############

tryon_canopy <- c(62, 64, 66, 68)

tryon_bd_source <- boot_backdoor(2500, canopy = tryon_canopy, 
                                      portland %>% filter(watershed != "Tryon Creek"))
tryon_bd_trans <- boot_backdoor(2500, canopy = tryon_canopy, 
                                     portland %>% filter(watershed != "Tryon Creek"), 
                                     portland %>% filter(watershed == "Tryon Creek"))
tryon_bd_target <- boot_backdoor(2500, canopy = tryon_canopy, 
                                      portland %>% filter(watershed == "Tryon Creek"))

tryon_fd_source <- boot_frontdoor(2500, canopy = tryon_canopy, 
                                       portland %>% filter(watershed != "Tryon Creek"))
tryon_fd_trans <- boot_frontdoor(2500, canopy = tryon_canopy, 
                                      portland %>% filter(watershed != "Tryon Creek"), 
                                      portland %>% filter(watershed == "Tryon Creek"))
tryon_fd_target <- boot_frontdoor(2500, canopy = tryon_canopy, 
                                       portland %>% filter(watershed == "Tryon Creek"))

plot_bootstrap_comparison(list(
  as.data.frame(tryon_bd_source),
  as.data.frame(tryon_bd_trans),
  as.data.frame(tryon_bd_target)
), tryon_canopy)

plot_bootstrap_comparison(list(
  as.data.frame(tryon_fd_source),
  as.data.frame(tryon_fd_trans),
  as.data.frame(tryon_fd_target)
), tryon_canopy)

############
# Kellogg Creek
############

kellogg_canopy <- c(35, 40, 45, 50)

kellogg_bd_source <- boot_backdoor(2500, canopy = kellogg_canopy, 
                                 portland %>% filter(watershed != "Kellogg Creek"))
kellogg_bd_trans <- boot_backdoor(2500, canopy = kellogg_canopy, 
                                portland %>% filter(watershed != "Kellogg Creek"), 
                                portland %>% filter(watershed == "Kellogg Creek"))
kellogg_bd_target <- boot_backdoor(2500, canopy = kellogg_canopy, 
                                 portland %>% filter(watershed == "Kellogg Creek"))

kellogg_fd_source <- boot_frontdoor(2500, canopy = kellogg_canopy, 
                                  portland %>% filter(watershed != "Kellogg Creek"))
kellogg_fd_trans <- boot_frontdoor(2500, canopy = kellogg_canopy, 
                                 portland %>% filter(watershed != "Kellogg Creek"), 
                                 portland %>% filter(watershed == "Kellogg Creek"))
kellogg_fd_target <- boot_frontdoor(2500, canopy = kellogg_canopy, 
                                  portland %>% filter(watershed == "Kellogg Creek"))

plot_bootstrap_comparison(list(
  as.data.frame(kellogg_bd_source),
  as.data.frame(kellogg_bd_trans),
  as.data.frame(kellogg_bd_target)
), kellogg_canopy)

plot_bootstrap_comparison(list(
  as.data.frame(kellogg_fd_source),
  as.data.frame(kellogg_fd_trans),
  as.data.frame(kellogg_fd_target)
), kellogg_canopy)