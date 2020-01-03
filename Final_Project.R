#+ setup, include=FALSE, echo=FALSE
knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.align = "center", warning=FALSE, message=FALSE)

#' # Poster Project: Investigating the Effect of Temperature on Exit Velocity and HR Distance

#' ### Libraries
library(SDSRegressionR)
library(tidyverse)
library(mosaic)
library(emmeans)
library(car)

#' ### Data
hr_temp <- read_csv("HR_Temp.csv")

# Factor Categorical Variable
tally(~pitch_name, data = hr_temp)

# Too many pitches to make all these dummy variables. I'll do an examination of fastballs vs. off-speed.
hr_temp <- hr_temp %>% 
  mutate(pitch_name_f = factor(pitch_name))

# Statistics
favstats(~hit_dist_sc, data = hr_temp)
favstats(~launch_speed, data = hr_temp)
favstats(~DailyAverageDryBulbTemperature, data = hr_temp)


# Correlation Test
library(psych)
hr_temp %>% 
  select(hit_dist_sc, launch_speed, launch_angle, DailyAverageDryBulbTemperature,
         DailyAverageStationPressure, release_speed) %>% 
  cor(use="pairwise.complete.obs")

tally(~pitch_name_f, data = hr_temp)

levels(hr_temp$pitch_name_f) <- list("Fastball" = c("2-Seam Fastball", "4-Seam Fastball", "Cutter", "Sinker", "Split Finger"),
                             "Off-Speed"= c("Changeup", "Curveball", "Knuckle Curve", "Slider"))

tally(~pitch_name_f, data = hr_temp)

# Alright. Now let's construct an initial model.

# Re-name variables to make it easier
distance = hr_temp$hit_dist_sc
pitch_speed = hr_temp$release_speed
pressure = hr_temp$DailyAverageStationPressure
precip = hr_temp$DailyPrecipitation
avg_temp = hr_temp$DailyAverageDryBulbTemperature
exit_velo = hr_temp$launch_speed

#fix precip
hr_temp$DailyPrecipitation <- as.numeric(hr_temp$DailyPrecipitation)
hr_temp$DailyPrecipitation[is.na(hr_temp$DailyPrecipitation)] <- 0

precip <- hr_temp$DailyPrecipitation

#' Some plots to get an idea of the relationships visually:
gf_point(distance ~ exit_velo) %>% 
  gf_lm()

gf_point(distance ~ pressure) %>% 
  gf_lm()

gf_point(distance ~ avg_temp) %>% 
  gf_lm()

#' ### Initial Model
initial_model <- lm(hit_dist_sc ~ release_speed + pitch_name_f + launch_angle + DailyAverageStationPressure + DailyPrecipitation +
                      DailyAverageDryBulbTemperature + launch_speed + DailyAverageDryBulbTemperature*launch_speed,
                    data = hr_temp)

summary(initial_model)

#' ### Diagnostics 
vif(initial_model) # Yikes!
residFitted(initial_model) # Looks good
cooksPlot(initial_model, key.variable = "X1", print.obs = T, save.cutoff = T)
3*cooksCutOff
# VIF looks rough.

#' ### Mean-Centering
hr_temp <- hr_temp %>% 
  mutate(exit_velo_cen = launch_speed - mean(launch_speed, na.rm = TRUE),
         temp_cen = DailyAverageDryBulbTemperature - mean(DailyAverageDryBulbTemperature, na.rm = TRUE))

m2 <- lm(hit_dist_sc ~ release_speed + pitch_name_f + launch_angle + DailyAverageStationPressure + DailyPrecipitation +
           temp_cen + exit_velo_cen + temp_cen*exit_velo_cen, data = hr_temp)  

#' Re-do the diagnostics:
vif(m2) # Much better. 
summary(m2)
residFitted(m2)
c <- cooksPlot(m2, key.variable = "X1", print.obs = T, save.cutoff = T)
2*cooksCutOff
c


#' ### Remove outliers. 
g_hr_temp <- hr_temp %>% 
  filter(X1 %not_in% c(c$X1[1:10]))

#' ### Final model
hr_f <- lm(hit_dist_sc ~ release_speed + pitch_name_f + launch_angle + DailyAverageStationPressure + DailyPrecipitation + 
             temp_cen + exit_velo_cen + temp_cen*exit_velo_cen, data = g_hr_temp)
summary(hr_f)

#' Grab the model data:
hr_model_data <- modelData(hr_f)
favstats(~temp_cen, data = hr_model_data) # Find moderator SD

#' ### Simple Slopes: Look at the mean +- 1 SD
minus_sd <- 0.10786 - 7.4208
plus_sd <- 0.10786 + 7.4208
mean_temp <- 0.10786

ref_grid(hr_f)

s_mns <- emmeans(hr_f, "exit_velo_cen", 
                 at = list(exit_velo_cen = c(0,1), temp_cen = c(minus_sd, mean_temp, plus_sd)),
                 by = "temp_cen")

s_mns
pairs(s_mns, reverse = TRUE)

#' ###  Regions of Significance
lmROS(hr_f, interest = "exit_velo_cen", moderator = "temp_cen")

#' ### Johnson-Neyman for simple slopes, ROS
library(interactions)
sim <- sim_slopes(hr_f, pred = "exit_velo_cen", modx = "temp_cen")
sim$slopes
johnson_neyman(hr_f, pred="exit_velo_cen", modx = "temp_cen")
interact_plot(hr_f, pred = "exit_velo_cen", modx = "temp_cen")

#' ### Standardized Betas
lmBeta(hr_f)

#' ### Partial Correlation Coefficients
pCorr(hr_f)
