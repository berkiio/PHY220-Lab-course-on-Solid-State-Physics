setwd("C:/Users/david/OneDrive/David/UZH Physik/HS25/Lab FKP")

#### Packages ####
# load relevant packages
pacman::p_load(tidyverse, signal, MASS, gridExtra, minpack.lm, rootSolve)

# check versions of used packages
pacman::p_version(tidyverse)
pacman::p_version(signal)
pacman::p_version(MASS)
pacman::p_version(gridExtra)
pacman::p_version(rootSolve)

#### Load data ####
first_cooling_slow = read.table("./Data/first_cooling_slow", header = T)
first_cooling_fast = read.table("./Data/first_cooling_fast", header = T)
first_heating_fast = read.table("./Data/first_heating_fast", header = T)
magnetic_cooling = read.table("./Data/magnet_cooling", header = T)
magnet_heating = read.table("./Data/magnet_heating", header = T)
super_slow_data = read.table("./Data/heating_super_slow", header = T)

#### Data preparation ####
# Function to do the data preparation
calculate_resistance = function(x,T_min,T_max){
  x %>%
    dplyr::filter(between(Tafter, T_min, T_max)) %>% 
    mutate(
      `res+` = `V.`/`I.`,
      `res-` = `V..1`/`I..1`, 
      Resistance = (`res+`+`res-`)/2)
}

dfs = list(first_cooling_slow, first_cooling_fast, first_heating_fast, magnetic_cooling, magnet_heating, super_slow_data)
data = pmap(list(dfs, 80, 120), calculate_resistance) # not sure about the pmap argument

#### Make plots ####
### Define graphic parameters ###
params = 
  theme_bw()+
  theme(
    plot.title = element_text(size = 36),
    axis.text = element_text(size = 34),
    axis.title = element_text(size = 34),
    strip.text = element_text(size = 30),
    legend.text = element_text(size = 34),
    legend.title = element_blank(),
    legend.position = c(.85,.15)
  )

### Plot voltage and resistance of 4 point measurement ###
# ggplot()+
#   geom_line()

### Generate the complete plot to show issues (email) ###
### Cooling
p1 = ggplot()+
  geom_line(data=data[[1]], aes(x=Tafter, y=Resistance, color="Slow_cooling"))+
  geom_line(data=data[[2]], aes(x=Tafter, y=Resistance, color="Fast_cooling"))+
  # geom_point(data=data[[3]], aes(x=Tafter, y=Resistance, color="Fast_heating"))+
  # geom_line(data=data[[4]], aes(x=Tafter, y=Resistance, color="Magnet_cooling"))+
  # geom_point(data=data[[5]], aes(x=Tafter, y=Resistance, color="Magnet_heating"))+
  theme_bw()+
  xlab("Temperature [K]") +
  ylab("Resistance [Ω]") + params

### plot the voltage measurements
p2 = ggplot()+
  geom_line(data=data[[1]], aes(x=Tafter, y=`V.`, color="slow cooling_V+"))+
  geom_line(data=data[[2]], aes(x=Tafter, y=`V.`, color="fast cooling_V+"))+
  geom_line(data=data[[1]], aes(x=Tafter, y=`V..1`, color="slow cooling_V-"))+
  geom_line(data=data[[2]], aes(x=Tafter, y=`V..1`, color="fast cooling_V-"))+
  ylab("Voltage [V]")+
  xlab("Temperature [K]")+
  theme_bw() + params

png(filename = "./PNG/Voltage_meas_issues.png", width=1920, height=1080, units="px")
grid.arrange(p1,p2,nrow=2)
dev.off()

### Complete resistance measurements plot ###
pt_size = 3
complete_resistance_plot = ggplot()+
  geom_point(data=data[[1]], aes(x=Tafter, y=Resistance, color="Slow cooling"), size=pt_size)+
  geom_point(data=data[[2]], aes(x=Tafter, y=Resistance, color="Fast cooling"), size=pt_size)+
  geom_point(data=data[[3]], aes(x=Tafter, y=Resistance, color="Fast heating"), size=pt_size)+
  geom_point(data=data[[4]], aes(x=Tafter, y=Resistance, color="Magnet cooling"), size=pt_size)+
  geom_point(data=data[[5]], aes(x=Tafter, y=Resistance, color="Magnet heating"), size=pt_size)+
  geom_point(data=data[[6]], aes(x=Tafter, y=Resistance, color="Super slow heating"), size=pt_size)+
  xlab("Temperature [K]") +
  ylab("Resistance [Ω]") + 
  params +
  theme(legend.title = element_blank())
  
png(filename = "./PNG/complete_resistance_plot.png", width=1920, height=1080, units="px")
complete_resistance_plot
dev.off()

### Emulated (corrected) complete resistance measurements plot ###
### since we have some fault data that seems to be shifted by a constant, we correct for this ###
# In a first step, we need to determine how much we are shifted 
# We do this by comparing our shifted data in sequences to our unshifted measurements

p1_shifted = subset(data[[1]], Tafter <= 92 & Resistance <= -0.0025)
p2_shifted = subset(data[[1]], Tafter > 93 & Resistance <= 0.0025)
p1_unshifted = subset(data[[2]], Tafter <= 92)
p2_unshifted = subset(data[[2]], Tafter > 93)

shiftp1 = abs(mean(p1_shifted$Resistance)-mean(p1_unshifted$Resistance))
shiftp2 = abs(mean(p2_shifted$Resistance)-mean(p2_unshifted$Resistance))
shift = (shiftp1+shiftp2)/2

data_slow_cool_corrected = data[[1]] %>%
  mutate(Resistance = ifelse(Tafter <= 92 & Resistance <= -0.0025, Resistance + shift, Resistance),
         Resistance = ifelse(Tafter > 93 & Resistance <= 0.0025, Resistance + shift, Resistance))

ggplot()+
  geom_point(data=data_slow_cool_corrected, aes(x=Tafter, y=Resistance, color="Slow cooling"), size=pt_size)+
  geom_point(data=data[[2]], aes(x=Tafter, y=Resistance, color="Fast cooling"), size=pt_size)+
  # geom_vline(xintercept = 93)+
  params + 
  theme(legend.title = element_blank())

### 4 point measurement data ###
ggplot()+
  geom_line(data=data[[1]], aes(x=Tafter, y=`V.`, color="slow cooling_V+"))+
  geom_line(data=data[[2]], aes(x=Tafter, y=`V.`, color="fast cooling_V+"))+
  geom_line(data=data[[1]], aes(x=Tafter, y=`V..1`, color="slow cooling_V-"))+
  geom_line(data=data[[2]], aes(x=Tafter, y=`V..1`, color="fast cooling_V-"))+
  ylab("Voltage [V]")+
  xlab("Temperature [K]")+
  theme_bw() + params

#### Uncertainty on time ####
# function to convert time format to numeric
convert_k_values <- function(df) {
  # Apply to every column
  df[] <- lapply(df, function(col) {
    # Only process if column is character or factor (since numeric won't have "k")
    if (is.factor(col)) col <- as.character(col)
    if (is.character(col)) {
      # Detect values like "1k", "2.5k", "-3k", etc.
      is_k <- grepl("^-?\\d*\\.?\\d+k$", col, ignore.case = TRUE)
      
      # Convert those to numeric * 1000
      col[is_k] <- as.numeric(sub("k$", "", col[is_k], ignore.case = TRUE)) * 1000
      
      # Try to convert entire column to numeric if possible
      suppressWarnings(col <- as.numeric(col))
    }
    return(col)
  })
  return(df)
}

time_data = read.table("./Data/probe_sensor_data.txt", header = T)
time_data = convert_k_values(time_data)
time_data = time_data %>%
  mutate(mean_dev = T_s-T_raw)

temp_uncert_plot = ggplot()+
  geom_point(data=subset(time_data, mean_dev!=0), aes(x = t, y = mean_dev, color="Mean deviation"), size=3) +
  geom_point(data=subset(time_data, mean_dev==0), aes(x = t, y = mean_dev, color="Mean"), size=3) +
  xlab("Time") +
  ylab("Temperature deviation") +
  params +
  theme(legend.title = element_blank())

png(filename = "./PNG/temp_uncert_estimation.png", width=1920, height=1080, units="px")
temp_uncert_plot
dev.off()

sd(time_data$mean_dev)
sqrt(0.25^2+sd(time_data$mean_dev)^2)

#### Resistance data directly from LabView ####
convert_to_ohms <- function(df) {
  df[] <- lapply(df, function(col) {
    # Convert factors to characters to avoid issues
    if (is.factor(col)) col <- as.character(col)
    
    if (is.character(col)) {
      # Handle milliohms (m)
      is_m <- grepl("^-?\\d*\\.?\\d+m$", col, ignore.case = TRUE)
      col[is_m] <- as.numeric(sub("m$", "", col[is_m], ignore.case = TRUE)) * 1e-3
      
      # Handle microohms (u)
      is_u <- grepl("^-?\\d*\\.?\\d+u$", col, ignore.case = TRUE)
      col[is_u] <- as.numeric(sub("u$", "", col[is_u], ignore.case = TRUE)) * 1e-6
      
      # Try converting remaining to numeric if possible
      suppressWarnings(col <- as.numeric(col))
    }
    
    return(col)
  })
  
  return(df)
}

resistance_data = read.table("./Data/resistance_magnet_cooling.txt", header = T)
resistance_data = convert_to_ohms(resistance_data)

# This plot verifies that the calculation of the resistance is correct and matches LabViews calculation
ggplot()+
  geom_point(data=data[[4]], aes(x=Tafter, y=Resistance, color="Magnet cooling"))+
  geom_point(data=resistance_data, aes(x=`T`, y=mohm, color="Magnet cooling LabView"))+
  theme_bw()

#### 2D smoothing algorithm ####
# input df (raw data), box-size in x-direction, box-size in y-direction
cluster_average = function(df, x_range = 2, y_range = 0.05) {
  results = data.frame(x_avg = numeric(), y_avg = numeric())
  
  for (i in seq_len(nrow(df))) {
    x0 = df$Tafter[i]
    y0 = df$Resistance[i]
    
    # Select points within box around (x0, y0)
    subset = df[
      df$Tafter >= (x0 - x_range/2) & df$Tafter <= (x0 + x_range/2) &
        df$Resistance >= (y0 - y_range/2) & df$Resistance <= (y0 + y_range/2),
    ]
    
    # Compute average if we have enough points
    if (nrow(subset) > 2) {
      results = rbind(
        results,
        data.frame(
          x_avg = mean(subset$Tafter, na.rm = TRUE),
          y_avg = mean(subset$Resistance, na.rm = TRUE)
        )
      )
    }
  }
  
  # Remove duplicates from overlap
  results = results[!duplicated(round(results, 3)), ]
  results
}

### Plot for slow cooling and heating ###
df_2 = data[[2]]
df_4 = data_slow_cool_corrected
df_4_unshifted = data[[1]]

smooth_path_2 = cluster_average(df_2, x_range=2, y_range=0.05)
smooth_path_4 = cluster_average(df_4, x_range=2, y_range=0.05)
smooth_path_unshifted_4 = cluster_average(df_4_unshifted, x_range=2, y_range=0.05)

unshift_shift_correction = ggplot() +
  geom_point(data=df_2, aes(Tafter, Resistance, color="Fast cooling"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_2, aes(x = x_avg, y = y_avg, color="Fast cooling"), linewidth = 1.2) +
  geom_point(data=df_4, aes(Tafter, Resistance, color="Slow cooling shifted"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_4, aes(x = x_avg, y = y_avg, color="Slow cooling shifted"), linewidth = 1.2) +
  geom_point(data=df_4_unshifted, aes(Tafter, Resistance, color="Slow cooling unshifted"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_unshifted_4, aes(x = x_avg, y = y_avg, color="Slow cooling unshifted"), linewidth = 1.2) +
  xlab("Temperature [K]") + 
  ylab("Resistance [Ω]") + 
  params

png(filename = "./PNG/unshift_shift_correction.png", width=1920, height=1080, units="px")
unshift_shift_correction
dev.off()

### Plot magnet cooling fast cooling slow cooling ###
df_2 = data[[2]] # This is fast cooling
df_4 = data_slow_cool_corrected # corrected slow cooling
df_magnet_cooling = data[[4]] # magnet cooling
 
smooth_path_2 = cluster_average(df_2, x_range=2, y_range=0.05)
smooth_path_4 = cluster_average(df_4, x_range=2, y_range=0.05)
smooth_path_df_magnet_cooling = cluster_average(df_magnet_cooling, x_range=2, y_range=0.05)

magnet_comp = ggplot() +
  geom_point(data=df_2, aes(Tafter, Resistance, color="Fast cooling"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_2, aes(x = x_avg, y = y_avg, color="Fast cooling"), linewidth = 1.2) +
  geom_point(data=df_4, aes(Tafter, Resistance, color="Corrected slow cooling"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_4, aes(x = x_avg, y = y_avg, color="Corrected slow cooling"), linewidth = 1.2) +
  geom_point(data=df_magnet_cooling, aes(Tafter, Resistance, color="Magnet cooling"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_df_magnet_cooling, aes(x = x_avg, y = y_avg, color="Magnet cooling"), linewidth = 1.2) +
  xlab("Temperature [K]") + 
  ylab("Resistance [Ω]") + 
  params

png(filename = "./PNG/magnet_comp.png", width=1920, height=1080, units="px")
magnet_comp
dev.off()

##### Heating offset #####
df_3 = data[[3]] # fast heating
df_5 = data[[5]] # magnet heating 
super_slow_data = data[[6]] # super slow heating

smooth_path_super_slow_data = cluster_average(super_slow_data)
smooth_path_3 = cluster_average(df_3, x_range=3)
smooth_path_5 = cluster_average(df_5, x_range=3)

heating_offset = ggplot() +
  geom_point(data=df_3, aes(Tafter, Resistance, color="Fast heating"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_3, aes(x = x_avg, y = y_avg, color="Fast heating"), linewidth = 1.2) +
  geom_point(data=df_5, aes(Tafter, Resistance, color="Magnet heating"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_5, aes(x = x_avg, y = y_avg, color="Magnet heating"), linewidth = 1.2) +
  geom_point(data=super_slow_data, aes(Tafter, Resistance, color="Super slow heating"), alpha = 0.4, size=1.5) +
  geom_path(data = smooth_path_super_slow_data, aes(x = x_avg, y = y_avg, color="Super slow heating"), linewidth = 1.2) +
  xlab("Temperature [K]") + 
  ylab("Resistance [Ω]") + 
  params

png(filename = "./PNG/heating_offset.png", width=1920, height=1080, units="px")
heating_offset
dev.off()

#### Find the critial temperature ####
df_2 = data[[2]] # This is fast cooling
df_4 = data_slow_cool_corrected # corrected slow cooling
df_magnet_cooling = data[[4]] # magnet cooling

smooth_path_2 = cluster_average(df_2, x_range=2, y_range=0.05)
smooth_path_4 = cluster_average(df_4, x_range=2, y_range=0.05)
smooth_path_df_magnet_cooling = cluster_average(df_magnet_cooling, x_range=2, y_range=0.05)

# fit a spline function
fit_spline_fast_cooling = smooth.spline(smooth_path_2$x_avg, smooth_path_2$y_avg, spar = 0.7) # spar controls smoothness with 0 being very wiggly
fit_spline_data_fast_cooling = data.frame(x_pos = fit_spline_fast_cooling[["data"]]$x, y_pos = fit_spline_fast_cooling[["data"]]$y)

fit_spline_slow_cooling = smooth.spline(smooth_path_4$x_avg, smooth_path_4$y_avg, spar = 0.7) # spar controls smoothness with 0 being very wiggly
fit_spline_data_slow_cooling = data.frame(x_pos = fit_spline_slow_cooling[["data"]]$x, y_pos = fit_spline_slow_cooling[["data"]]$y)

fit_spline_magnet_cooling = smooth.spline(smooth_path_df_magnet_cooling$x_avg, smooth_path_df_magnet_cooling$y_avg, spar = 0.7) # spar controls smoothness with 0 being very wiggly
fit_spline_data_magnet_cooling = data.frame(x_pos = fit_spline_magnet_cooling[["data"]]$x, y_pos = fit_spline_magnet_cooling[["data"]]$y)

# Find temperature where R crosses 0 (critical temperature)
# Fast cooling
R_fun <- function(T) predict(fit_spline_fast_cooling, T)$y
Tc_zero_fast_cooling <- uniroot(R_fun, lower = min(smooth_path_2$x_avg), upper = max(smooth_path_2$x_avg))$root
Tc_zero_fast_cooling

# Slow cooling
R_fun <- function(T) predict(fit_spline_slow_cooling, T)$y
Tc_zero_slow_cooling <- uniroot(R_fun, lower = min(smooth_path_4$x_avg), upper = max(smooth_path_4$x_avg))$root
Tc_zero_slow_cooling

# Magnet cooling
R_fun <- function(T) predict(fit_spline_magnet_cooling, T)$y
Tc_zero_magnet_cooling <- uniroot(R_fun, lower = min(smooth_path_df_magnet_cooling$x_avg), upper = max(smooth_path_df_magnet_cooling$x_avg))$root
Tc_zero_magnet_cooling


crit_temp_est = ggplot()+
  geom_line(data=fit_spline_data_fast_cooling, aes(x=x_pos, y=y_pos, color="Fast cooling polynomial fit"))+
  geom_point(data=fit_spline_data_fast_cooling, aes(x=x_pos, y=y_pos, color="Fast cooling polynomial fit"))+
  geom_segment(aes(x=Tc_zero_fast_cooling, xend=Tc_zero_fast_cooling, y=-0.003, yend=0.004, color="Fast cooling polynomial fit"))+
  geom_line(data=fit_spline_data_slow_cooling, aes(x=x_pos, y=y_pos, color="Slow cooling polynomial fit"))+
  geom_point(data=fit_spline_data_slow_cooling, aes(x=x_pos, y=y_pos, color="Slow cooling polynomial fit"))+
  geom_segment(aes(x=Tc_zero_slow_cooling, xend=Tc_zero_slow_cooling, y=-0.003, yend=0.004, color="Slow cooling polynomial fit"))+
  geom_line(data=fit_spline_data_magnet_cooling, aes(x=x_pos, y=y_pos, color="Magnet cooling polynomial fit"))+
  geom_point(data=fit_spline_data_magnet_cooling, aes(x=x_pos, y=y_pos, color="Magnet cooling polynomial fit"))+
  geom_segment(aes(x=Tc_zero_magnet_cooling, xend=Tc_zero_magnet_cooling, y=-0.003, yend=0.004, color="Magnet cooling polynomial fit"))+
  annotate("text", x=Tc_zero_fast_cooling, y=0.0045, label="92.665K", color="#F8766D", angle = 45, size = 12) +
  annotate("text", x=Tc_zero_slow_cooling+0.5, y=0.0045, label="92.817K", color="#619CFF", angle = 45, size = 12) +
  annotate("text", x=Tc_zero_magnet_cooling, y=0.0045, label="92.047K", color="#00BA38", angle = 45, size = 12) +
  theme_bw() +
  xlab("Temperature [K]") + 
  ylab("Resistance [Ω]") + 
  coord_cartesian(expand = F) +
  params


png(filename = "./PNG/crit_temp_est.png", width=1920, height=1080, units="px")
crit_temp_est
dev.off()


