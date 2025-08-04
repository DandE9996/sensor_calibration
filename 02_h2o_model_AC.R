###############################################################################
# 1) SETUP: Load Libraries, Clear Workspace, and Set Config
###############################################################################
library(openair)
library(h2o)
library(httr)
library(dplyr)

# Optional: Set your working directory
#setwd("D:/OneDrive - University of Birmingham/Sensor Calibration/Sensor_Calibration/AC")

# Clear existing objects
rm(list = ls(all = TRUE))

# Configure HTTP request timeout to 10 minutes
set_config(config(timeout = 6000))


###############################################################################
# 2) READ DATA AND PREPARE
###############################################################################
data <- read.csv("D:/OneDrive - University of Birmingham/Sensor_calibration/AC/merge_AC.csv")

data <- data %>%
  select(-date, -X) %>%
  mutate(
    ID        = row_number(),
    pm02_refs = (pm02_no.001 + pm02_no.002 + pm02_no.004 + pm02_no.003 + pm02_no.005) / 5
  )

target <- "pm02"
predictors <- c("pm02_refs", "rhum", "atmp")

###############################################################################
# 3) DEFINE FUNCTION TO SPLIT DATA BY pm02 THRESHOLD
###############################################################################
split_data_by_pm02 <- function(data, thresholds) {
  n <- length(thresholds) + 1
  split_list <- vector("list", n)
  for (i in seq_len(n)) {
    if (i == 1) {
      split_list[[i]] <- data %>% filter(pm02_refs < thresholds[1])
    } else if (i == n) {
      split_list[[i]] <- data %>% filter(pm02_refs >= thresholds[n - 1])
    } else {
      split_list[[i]] <- data %>% filter(pm02_refs >= thresholds[i - 1] & pm02_refs < thresholds[i])
    }
  }
  split_list
}

# Split into 2 groups at pm02 = 50
split_data <- split_data_by_pm02(data, thresholds = c(50))

###############################################################################
# 4) INITIALIZE H2O AND PREPARE TRAIN/TEST SPLITS
###############################################################################
h2o.init(max_mem_size = "64G")

h2o_splits <- lapply(split_data, as.h2o)
split_frames <- lapply(h2o_splits, function(df) {
  h2o.splitFrame(data = df, ratios = 0.8, seed = 1014)
})
train_sets <- lapply(split_frames, `[[`, 1)
test_sets  <- lapply(split_frames, `[[`, 2)

###############################################################################
# 5) RUN H2O AUTOML (REGRESSION) FOR EACH SUBSET
###############################################################################
automl_models <- list()
for (i in seq_along(train_sets)) {
  automl_models[[i]] <- h2o.automl(
    x               = predictors,
    y               = target,
    training_frame  = train_sets[[i]],
    max_models      = 6,
    seed            = 1014
  )
}

###############################################################################
# 6) PREDICT USING THE BEST MODELS ON EACH SUBSET
###############################################################################
for (i in seq_along(split_data)) {
  h2o_data   <- as.h2o(split_data[[i]])
  best_model <- h2o.get_best_model(automl_models[[i]], criterion = "auto")
  pred       <- h2o.predict(best_model, h2o_data)
  pred_df    <- as.data.frame(pred)
  split_data[[i]] <- cbind(split_data[[i]], predict = pred_df$predict)
}

###############################################################################
# 7) MERGE ALL SUBSETS BACK AND SORT BY ID
###############################################################################
merged_data <- do.call(rbind, split_data) %>% arrange(ID)

###############################################################################
# 8) SAVE THE TWO BEST MODELS TO DISK (low/high)
###############################################################################
save_folder <- "D:/OneDrive - University of Birmingham/Sensor_calibration/AC/models"
model_names <- c("lowlvl", "highlvl")

for (i in seq_along(automl_models)) {
  best_model      <- h2o.get_best_model(automl_models[[i]], criterion = "auto")
  this_model_name <- paste0("modCalib_", model_names[i])
  model_path      <- h2o.saveModel(
    object   = best_model,
    path     = save_folder,
    force    = TRUE,
    filename = this_model_name
  )
  cat(sprintf("'%s' model saved at: %s\n", this_model_name, model_path))
}

###############################################################################
# 9) PLOTTING EXAMPLES (Scatter + Time Series)
###############################################################################
###############################################################################
# 12) FIGURE - SCATTER PLOT (100-unit ticks from 50 to 600, bold axis titles)
###############################################################################
#png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AC_ref_calibration_50.png", 
#    width = 7, height = 7, units = "in", res = 600)

# Save the current graphical parameters
old_par <- par(no.readonly = TRUE)

# Set up square plot with larger text and bold axis titles
par(
  pty      = "s",               # square region
  mar      = c(4.5, 4.5, 2, 2), # margins
  cex.axis = 1.5,               # tick-label size
  cex.lab  = 1.8,               # axis-title size
  mgp      = c(3, 1, 0),        # margin lines for labels
  lwd      = 1.5,
  font.lab = 2
)

axis_limit  <- c(0, 50)
custom_ticks <- seq(0, 50, by = 10)

# Filter data to 0-50 range for pm02
filtered_data <- merged_data[merged_data$pm02 >= 0 & merged_data$pm02 <= 50, ]

# Base scatter, suppress default axes
plot(
  merged_data$pm02, merged_data$pm02_refs,
  xlim   = axis_limit, ylim   = axis_limit,
  xlab   = "Fidas 200 (µg m⁻³)", 
  ylab   = "AC Drift-Reference (µg m⁻³)",
  pch    = 16, col     = "#1975BA",
  cex    = 1.5,            # larger points
  asp    = 1,              # 1:1 aspect ratio
  xaxt   = "n",            # no default x-axis
  yaxt   = "n"             # no default y-axis
)

# Custom axes at 100-unit intervals
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)

# Box around plot
box(lwd = 1.5)

# Overlay calibrated predictions
points(
  merged_data$pm02, merged_data$predict,
  pch = 17, col = "#F47F72", cex = 1.5
)

# 1:1 dashed line
abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

# Regression lines forced through origin - USING FILTERED DATA (0-50 range only)
model_sensor  <- lm(pm02_refs ~ 0 + pm02, data = filtered_data)
model_predict <- lm(predict   ~ 0 + pm02, data = filtered_data)

# Print the slopes for the 0-50 range
cat("Slope for Observed (0-50 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (0-50 range):", coef(model_predict), "\n")

abline(a = 0, b = coef(model_sensor),  col = "#1975BA", lwd = 2.5)
abline(a = 0, b = coef(model_predict), col = "#F47F72", lwd = 2.5)

# Legend with larger text
legend(
  "topleft",
  legend = c("Raw", "Calibrated"),
  col    = c("#1975BA", "#F47F72"),
  pch    = c(16, 17),
  bty    = "n",         # no box
  cex    = 1.6,         # text size
  pt.cex = 1.6,         # symbol size
  inset  = 0.00
)

dev.off()

###############################################################################
# 12) FIGURE - SCATTER PLOT (100-unit ticks from 50 to 600, bold axis titles)
###############################################################################
#png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AC_ref_calibration_600.png", 
#    width = 7, height = 7, units = "in", res = 600)

# Save the current graphical parameters
old_par <- par(no.readonly = TRUE)

# Set up square plot with larger text and bold axis titles
par(
  pty      = "s",               # square region
  mar      = c(4.5, 4.5, 2, 2), # margins
  cex.axis = 1.5,               # tick-label size
  cex.lab  = 1.8,               # axis-title size
  mgp      = c(3, 1, 0),        # margin lines for labels
  lwd      = 1.5,
  font.lab = 2
)

axis_limit  <- c(50, 600)
custom_ticks <- seq(50, 600, by = 100)

# Filter data to 0-50 range for pm02
filtered_data <- merged_data[merged_data$pm02 >= 50 & merged_data$pm02 <= 600, ]

# Base scatter, suppress default axes
plot(
  merged_data$pm02, merged_data$pm02_refs,
  xlim   = axis_limit, ylim   = axis_limit,
  xlab   = "Fidas 200 (µg m⁻³)", 
  ylab   = "AC Drift-Reference (µg m⁻³)",
  pch    = 16, col     = "#1975BA",
  cex    = 1.5,            # larger points
  asp    = 1,              # 1:1 aspect ratio
  xaxt   = "n",            # no default x-axis
  yaxt   = "n"             # no default y-axis
)

# Custom axes at 100-unit intervals
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)

# Box around plot
box(lwd = 1.5)

# Overlay calibrated predictions
points(
  merged_data$pm02, merged_data$predict,
  pch = 17, col = "#F47F72", cex = 1.5
)

# 1:1 dashed line
abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

# Regression lines forced through origin - USING FILTERED DATA (0-50 range only)
model_sensor  <- lm(pm02_refs ~ 0 + pm02, data = filtered_data)
model_predict <- lm(predict   ~ 0 + pm02, data = filtered_data)

# Print the slopes for the 0-50 range
cat("Slope for Observed (0-50 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (0-50 range):", coef(model_predict), "\n")

abline(a = 0, b = coef(model_sensor),  col = "#1975BA", lwd = 2.5)
abline(a = 0, b = coef(model_predict), col = "#F47F72", lwd = 2.5)

# Legend with larger text
legend(
  "topleft",
  legend = c("Raw", "Calibrated"),
  col    = c("#1975BA", "#F47F72"),
  pch    = c(16, 17),
  bty    = "n",         # no box
  cex    = 1.6,         # text size
  pt.cex = 1.6,         # symbol size
  inset  = 0.00
)

dev.off()

# Set up PNG device for the time series plot
#png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AC_calibration_timeseries.png", 
#    width = 10, height = 6, units = "in", res = 600)

# Set up time series plot with increased text sizes
par(mar = c(4.5, 4.5, 3, 2),  # Increased margins for larger labels
    cex.axis = 1.5,       # Larger axis numbers
    cex.lab = 1.8,        # Larger axis labels
    cex.main = 2.0,       # Larger title
    mgp = c(3, 1, 0),     # Adjust margin line for axis labels
    lwd = 1.5)            # Thicker lines overall

# Time Series
axis_limit_ts <- c(0, 750)

plot(merged_data$ID,
     merged_data$pm02, 
     type = "l",  
     col = "#1975BA",
     lwd = 2.5,              # Thicker line
     xlim = range(merged_data$ID),
     ylim = axis_limit_ts,
     xlab = "Timeseries",
     ylab = "PM2.5 (µg/m³)",
     main = "Time Series: Fidas vs Sensor Calibration")

# Add other lines with thicker width
lines(merged_data$ID, 
      merged_data$predict, 
      col = "#F47F72", 
      lwd = 2.5,             # Thicker line 
      lty = 1)

lines(merged_data$ID, 
      merged_data$pm02_refs, 
      col = "black", 
      lwd = 2.5,             # Thicker line
      lty = 2)

# Add legend with larger text
legend("topright", 
       legend = c("Fidas (pm02)", "Post-calibration", "Pre-Calibration"),
       col    = c("#1975BA", "#F47F72", "black"),
       lty    = c(1, 1, 2),
       lwd    = 2.5,
       bty    = "n",         # No box around legend
       cex    = 1.8,         # Larger legend text
       inset  = 0.05)        # Adjust position inside plot

# Add grid with thicker lines
grid(lwd = 1.5)

# Close the device to save the time series plot
#dev.off()

# Restore original plotting parameters
par(old_par)

###############################################################################
# 10) COMPUTE AND DISPLAY PERFORMANCE METRICS (Pre vs Post)
###############################################################################
# B. Stats helper function
calc_stats <- function(obs, pred) {
  valid_idx <- complete.cases(obs, pred)
  obs  <- obs[valid_idx]
  pred <- pred[valid_idx]
  R2   <- cor(obs, pred)^2
  RMSE <- sqrt(mean((obs - pred)^2))
  MAE  <- mean(abs(obs - pred))
  MBE  <- mean(obs - pred)
  
  # Use openair modStats() to calculate IOA
  # Create a temporary data frame for modStats
  temp_df <- data.frame(obs = obs, mod = pred)
  mod_stats <- modStats(temp_df, obs = "obs", mod = "mod")
  IOA <- mod_stats$IOA
  
  data.frame(
    N    = length(obs),
    R2   = R2,
    RMSE = RMSE,
    MAE  = MAE,
    MBE  = MBE,
    IOA  = IOA
  )
}

# C. Pre-calibration (pm02_refs vs pm02)
pre_low  <- calc_stats(obs = split_data[[1]]$pm02, pred = split_data[[1]]$pm02_refs)
pre_high <- calc_stats(obs = split_data[[2]]$pm02, pred = split_data[[2]]$pm02_refs)
pre_all  <- calc_stats(obs = merged_data$pm02,     pred = merged_data$pm02_refs)

# D. Post-calibration (predict vs pm02)
post_low  <- calc_stats(obs = split_data[[1]]$pm02, pred = split_data[[1]]$predict)
post_high <- calc_stats(obs = split_data[[2]]$pm02, pred = split_data[[2]]$predict)
post_all  <- calc_stats(obs = merged_data$pm02,     pred = merged_data$predict)

# E. Combine pre & post metrics side by side
#    Rename columns for clarity: N_pre, R2_pre, ... vs N_post, R2_post, ...
rename_stats <- function(df, prefix) {
  names(df) <- paste0(names(df), "_", prefix)
  df
}

pre_low  <- rename_stats(pre_low,   "pre")
pre_high <- rename_stats(pre_high,  "pre")
pre_all  <- rename_stats(pre_all,   "pre")

post_low  <- rename_stats(post_low,  "post")
post_high <- rename_stats(post_high, "post")
post_all  <- rename_stats(post_all,  "post")

df_low  <- cbind(Subset = "Low (<50 µg/m³)",  pre_low,  post_low)
df_high <- cbind(Subset = "High (≥50 µg/m³)", pre_high, post_high)
df_all  <- cbind(Subset = "All Data",         pre_all,  post_all)

df_comparison <- rbind(df_low, df_high, df_all)

# F. Round all numeric columns to 2 decimals
df_comparison[] <- lapply(df_comparison, function(x) if(is.numeric(x)) round(x, 2) else x)

# G. Print in console
print(df_comparison)

###############################################################################
# 11) OPTIONAL: PROVIDE A MARKDOWN TABLE
###############################################################################
cat("\n### Markdown Table (Pre-Cal vs Post-Cal) ###\n\n")
cat("| Subset | N_pre | R2_pre | RMSE_pre | MAE_pre | MBE_pre | IOA_pre | N_post | R2_post | RMSE_post | MAE_post | MBE_post | IOA_post |\n")
cat("|--------|-------|--------|----------|---------|---------|---------|--------|---------|----------|----------|---------|----------|\n")

for (i in seq_len(nrow(df_comparison))) {
  row <- df_comparison[i, ]
  cat(
    "|", row$Subset,
    "|", row$N_pre,
    "|", row$R2_pre,
    "|", row$RMSE_pre,
    "|", row$MAE_pre,
    "|", row$MBE_pre,
    "|", row$IOA_pre,
    "|", row$N_post,
    "|", row$R2_post,
    "|", row$RMSE_post,
    "|", row$MAE_post,
    "|", row$MBE_post,
    "|", row$IOA_post, 
    "|\n"
  )
}