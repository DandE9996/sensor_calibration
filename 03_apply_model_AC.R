###############################################################################
# 0) LOAD LIBRARIES AND H2O MODELS
###############################################################################
library(h2o)
library(dplyr)
library(ggplot2)
library(openair)  # Added for modStats() function

# Initialize or connect to H2O
h2o.init()

# Load previously saved models (low-level and high-level)
model_low  <- h2o.loadModel("D:/OneDrive - University of Birmingham/Sensor_calibration/AC/models/modCalib_lowlvl")
model_high <- h2o.loadModel("D:/OneDrive - University of Birmingham/Sensor_calibration/AC/models/modCalib_highlvl")

###############################################################################
# 1) READ AND PREPARE THE DATA
###############################################################################
data <- read.csv("D:/OneDrive - University of Birmingham/Sensor_calibration/AC/merge_AC_test.csv")

# Use "pm02" as the reference from Fidas, "pm02_no.011" as uncalibrated sensor,
# and "predict" in the CSV as some drift-reference or placeholder. 
# We'll rename "predict" as "pm02_refs" for clarity, 
# which stands for a 'drift-reference-based' PM2.5 if it existed.
data <- data %>%
  select(-date, -X) %>%
  mutate(
    ID        = row_number(),
    pm02_refs = predict  # rename 'predict' column from CSV to 'pm02_refs'
  ) %>% 
  select(-predict)

target <- "pm02"
predictors <- c("pm02_refs", "rhum", "atmp")

###############################################################################
# 2) SPLIT THE DATA BY pm02_refs (< 50 OR >= 50)
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

# Split into two subsets at pm02_refs = 50
split_data <- split_data_by_pm02(data, thresholds = c(50))

# We have two H2O models corresponding to low and high subsets
automl_models <- list(model_low, model_high)

###############################################################################
# 3) PREDICT ON EACH SUBSET
###############################################################################
for (i in seq_along(split_data)) {
  h2o_data  <- as.h2o(split_data[[i]])
  this_model <- automl_models[[i]]
  
  # Predict using the best model for this bin
  pred <- h2o.predict(this_model, h2o_data)
  pred_df <- as.data.frame(pred)
  
  # Attach predictions to the original subset in R
  split_data[[i]] <- cbind(split_data[[i]], predict = pred_df$predict)
}

# Merge subsets back and sort by ID
merged_data <- do.call(rbind, split_data) %>% arrange(ID)

###############################################################################
# 4) MODIFIED PLOT: SQUARE WITHOUT DRIFT-REFERENCE AND NO GRID LINES
#    (with 100-unit ticks and bold axis titles)
###############################################################################

png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AC_post_50.png", 
    width = 7, height = 7, units = "in", res = 600)

# Save current graphical parameters
old_par <- par(no.readonly = TRUE)

# Square plot, larger text, bold axis titles
par(
  pty      = "s",               # square region
  mar      = c(4.5, 4.5, 2, 2), # margins
  cex.axis = 1.5,               # tick-label size
  cex.lab  = 1.8,               # axis-title size
  mgp      = c(3, 1, 0),        # margin lines for labels
  lwd      = 1.5,               # line width
  font.lab = 2                  # bold axis titles
)

axis_limit <- c(0, 50)
custom_ticks <- seq(0, 50, by = 10)

# Filter data to 0-50 range for pm02
filtered_data <- merged_data[merged_data$pm02 >= 0 & merged_data$pm02 <= 50, ]

# 1. Scatter: pm02 vs pm02_no.006, suppress default axes
plot(
  merged_data$pm02, 
  merged_data$pm02_no.006,
  xlim   = axis_limit, 
  ylim   = axis_limit, 
  xlab   = "Fidas 200 (µg m⁻³)", 
  ylab   = "AC Field Sensor (µg m⁻³)",
  pch    = 16, 
  col    = "#1975BA",
  asp    = 1,       # 1:1 aspect ratio
  cex    = 1.5,     # point size
  xaxt   = "n",     # no default x-axis
  yaxt   = "n"      # no default y-axis
)

# 2. Custom axes at 100-unit intervals
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)

# 3. Box around plot
box(lwd = 1.5)

# 4. Overlay calibrated predictions
points(
  merged_data$pm02, 
  merged_data$predict, 
  pch = 17, 
  col = "#BA3E45",
  cex = 1.5
)

# 5. 1:1 dashed line
abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

# 6. Trend lines (through origin) - USING FILTERED DATA (0-50 range only)
model_sensor <- lm(pm02_no.011 ~ 0 + pm02, data = filtered_data)
model_calib  <- lm(predict     ~ 0 + pm02, data = filtered_data)

# Print the slopes for the 0-50 range
cat("Slope for Observed (0-50 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (0-50 range):", coef(model_calib), "\n")

abline(model_sensor, col = "#1975BA", lwd = 2.5)
abline(model_calib,  col = "#BA3E45", lwd = 2.5)

# 7. Legend
legend(
  "topleft",
  legend = c("Raw", "Calibrated"),
  col    = c("#1975BA", "#BA3E45"),
  pch    = c(16, 17),
  cex    = 1.6,   # text size
  pt.cex = 1.6,   # symbol size
  bty    = "n",   # no box
  inset  = 0.00
)

dev.off()

###############################################################################
# 4) MODIFIED PLOT: SQUARE WITHOUT DRIFT-REFERENCE AND NO GRID LINES
#    (with 100-unit ticks and bold axis titles)
###############################################################################

png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AC_post_600.png", 
    width = 7, height = 7, units = "in", res = 600)

# Save current graphical parameters
old_par <- par(no.readonly = TRUE)

# Square plot, larger text, bold axis titles
par(
  pty      = "s",               # square region
  mar      = c(4.5, 4.5, 2, 2), # margins
  cex.axis = 1.5,               # tick-label size
  cex.lab  = 1.8,               # axis-title size
  mgp      = c(3, 1, 0),        # margin lines for labels
  lwd      = 1.5,               # line width
  font.lab = 2                  # bold axis titles
)

axis_limit <- c(50, 600)
custom_ticks <- seq(50, 600, by = 100)

# Filter data to 0-50 range for pm02
filtered_data <- merged_data[merged_data$pm02 >= 50 & merged_data$pm02 <= 600, ]

# 1. Scatter: pm02 vs pm02_no.006, suppress default axes
plot(
  merged_data$pm02, 
  merged_data$pm02_no.006,
  xlim   = axis_limit, 
  ylim   = axis_limit, 
  xlab   = "Fidas 200 (µg m⁻³)", 
  ylab   = "AC Field Sensor (µg m⁻³)",
  pch    = 16, 
  col    = "#1975BA",
  asp    = 1,       # 1:1 aspect ratio
  cex    = 1.5,     # point size
  xaxt   = "n",     # no default x-axis
  yaxt   = "n"      # no default y-axis
)

# 2. Custom axes at 100-unit intervals
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)

# 3. Box around plot
box(lwd = 1.5)

# 4. Overlay calibrated predictions
points(
  merged_data$pm02, 
  merged_data$predict, 
  pch = 17, 
  col = "#BA3E45",
  cex = 1.5
)

# 5. 1:1 dashed line
abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

# 6. Trend lines (through origin) - USING FILTERED DATA (0-50 range only)
model_sensor <- lm(pm02_no.011 ~ 0 + pm02, data = filtered_data)
model_calib  <- lm(predict     ~ 0 + pm02, data = filtered_data)

# Print the slopes for the 0-50 range
cat("Slope for Observed (0-50 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (0-50 range):", coef(model_calib), "\n")

abline(model_sensor, col = "#1975BA", lwd = 2.5)
abline(model_calib,  col = "#BA3E45", lwd = 2.5)

# 7. Legend
legend(
  "topleft",
  legend = c("Raw", "Calibrated"),
  col    = c("#1975BA", "#BA3E45"),
  pch    = c(16, 17),
  cex    = 1.6,   # text size
  pt.cex = 1.6,   # symbol size
  bty    = "n",   # no box
  inset  = 0.00
)

dev.off()
###############################################################################
# 5) CALCULATE STATISTICS
###############################################################################
calc_stats <- function(df, ref_col = "pm02", pred_col) {
  obs  <- df[[ref_col]]
  pred <- df[[pred_col]]
  
  # Remove missing
  good_idx <- complete.cases(obs, pred)
  obs  <- obs[good_idx]
  pred <- pred[good_idx]
  
  N    <- length(obs)
  r2   <- summary(lm(pred ~ obs))$r.squared
  rmse <- sqrt(mean((pred - obs)^2))
  mae  <- mean(abs(pred - obs))
  mbe  <- mean(pred - obs)
  
  # Use openair modStats() to calculate IOA
  # Create a temporary data frame for modStats
  temp_df <- data.frame(obs = obs, mod = pred)
  mod_stats <- modStats(temp_df, obs = "obs", mod = "mod")
  ioa <- mod_stats$IOA
  
  data.frame(N = N, R2 = r2, RMSE = rmse, MAE = mae, MBE = mbe, IOA = ioa)
}

# We'll gather stats for:
# 1) Pre-calibration: pm02_no.011 vs pm02
# 2) Post-calibration: predict vs pm02
# 3) Drift-reference: pm02_refs vs pm02

get_stats_table <- function(df, label) {
  pre_cal    <- calc_stats(df, ref_col = "pm02", pred_col = "pm02_no.011")
  post_cal   <- calc_stats(df, ref_col = "pm02", pred_col = "predict")
  drift_refs <- calc_stats(df, ref_col = "pm02", pred_col = "pm02_refs")
  
  tbl <- rbind(
    "Pre-Calibration"       = pre_cal,
    "Post-Calibration"      = post_cal,
    "Drift-Reference"       = drift_refs
  )
  tbl$Subset <- label
  tbl
}

# Stats for each bin + all
stats_low  <- get_stats_table(split_data[[1]], label = "Low Bin")
stats_high <- get_stats_table(split_data[[2]], label = "High Bin")
stats_all  <- get_stats_table(merged_data,     label = "All Data")

# Combine them all
stats_combined <- rbind(stats_low, stats_high, stats_all)
stats_combined <- stats_combined[, c("Subset", "N", "R2", "RMSE", "MAE", "MBE", "IOA")]

# Round to 2 decimals
stats_combined[] <- lapply(stats_combined, function(x) if(is.numeric(x)) round(x, 2) else x)

cat("## Statistics for Low Bin ##\n")
print(stats_low)

cat("\n## Statistics for High Bin ##\n")
print(stats_high)

cat("\n## Statistics for All Data ##\n")
print(stats_all)

cat("\n## Combined Statistics (All Bins + All Data) ##\n")
print(stats_combined)

###############################################################################
# 6) MARKDOWN TABLE
###############################################################################
cat("\n### Markdown Table (Pre-Cal, Post-Cal, Drift-Reference) ###\n\n")
cat("| Subset | Method            | N   | R2   | RMSE | MAE  | MBE  | IOA  |\n")
cat("|--------|-------------------|-----|------|------|------|------|------|\n")

for (i in seq_len(nrow(stats_combined))) {
  row <- stats_combined[i, ]
  cat(
    "|", row$Subset,
    "|", rownames(stats_combined)[i],
    "|", row$N,
    "|", row$R2,
    "|", row$RMSE,
    "|", row$MAE,
    "|", row$MBE,
    "|", row$IOA,
    "|\n"
  )
}