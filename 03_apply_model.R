###############################################################################
# 0) LOAD LIBRARIES AND H2O MODELS
###############################################################################
library(h2o)
library(dplyr)
library(ggplot2)
library(openair)  # Added for modStat() function

# Start or connect to your H2O cluster (if not already running)
h2o.init()

# Adjust these paths to where you saved your models in Step 2
model_low  <- h2o.loadModel("D:/OneDrive - University of Birmingham/Sensor_calibration/models/modCalib_lowlvl")
model_high <- h2o.loadModel("D:/OneDrive - University of Birmingham/Sensor_calibration/models/modCalib_highlvl")

###############################################################################
# 1) READ THE COMBINED CSV FROM STEP 1
###############################################################################
data <- read.csv("D:/OneDrive - University of Birmingham/Sensor_calibration/merge_AG_test.csv")

data <- data %>%
  select(-date, -X) %>%
  mutate(
    ID = row_number(),
    pm02_refs  = predict
  ) %>% 
  select(-predict)

target <- "pm02"
predictors <- c("pm02_refs", "rhum", "atmp")

###############################################################################
# 2) SPLIT DATA INTO CONCENTRATION BINS
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

split_data <- split_data_by_pm02(data, thresholds = c(50))
automl_models <- list(model_low, model_high)

###############################################################################
# 6) PREDICT USING THE BEST MODELS ON EACH SUBSET
###############################################################################
for (i in seq_along(split_data)) {
  h2o_data <- as.h2o(split_data[[i]])
  auto_model <- automl_models[[i]]
  pred <- h2o.predict(auto_model, h2o_data)
  
  pred_df <- as.data.frame(pred)
  split_data[[i]] <- cbind(split_data[[i]], pred_df)
}

###############################################################################
# 7) MERGE ALL SUBSETS BACK AND SORT BY ID
###############################################################################
merged_data <- do.call(rbind, split_data)
merged_data <- merged_data %>% arrange(ID)

###############################################################################
# MODIFIED PLOTTING CODE FOR SQUARE PLOT WITHOUT DRIFT-REFERENCE POINTS
###############################################################################
#png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AG_post_50.png", 
#    width = 7, height = 7, units = "in", res = 600)

# Save current graphical parameters
old_par <- par(no.readonly = TRUE)

# Set up square plot with larger text and bold axis titles
par(
  pty      = "s",               # square plot region
  mar      = c(4.5, 4.5, 2, 2), # margins
  cex.axis = 1.5,               # tick-label size
  cex.lab  = 1.8,               # axis-title size
  mgp      = c(3, 1, 0),        # margin lines for labels
  lwd      = 1.5,               # line thickness
  font.lab = 2                  # bold axis titles
)

axis_limit <- c(0, 50)
custom_ticks <- seq(0, 50, by = 10)

# Filter data to 0-50 range for pm02
filtered_data <- merged_data[merged_data$pm02 >= 0 & merged_data$pm02 <= 50, ]

# 1. Scatter: pm02 vs pm02_no.11, with default axes suppressed
plot(
  merged_data$pm02, 
  merged_data$pm02_no.11, 
  xlim   = axis_limit, 
  ylim   = axis_limit, 
  xlab   = "Fidas 200 (µg m⁻³)", 
  ylab   = "AG Field Sensor (µg m⁻³)",
  pch    = 16, 
  col    = "#1975BA",
  asp    = 1,       # 1:1 aspect ratio
  cex    = 1.5,     # point size
  xaxt   = "n",     # suppress x-axis
  yaxt   = "n"      # suppress y-axis
)

# 2. Draw only our custom axes at 100-unit intervals
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)

# 3. Box around the plot
box(lwd = 1.5)

# 4. Overlay calibrated points
points(
  merged_data$pm02, 
  merged_data$predict, 
  pch = 17, 
  col = "#BA3E45",
  cex = 1.5
)

# 5. 1:1 dashed line
abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

# 6. Trend lines through origin - USING FILTERED DATA (0-50 range only)
model_sensor  <- lm(pm02_no.11 ~ 0 + pm02, data = filtered_data)  
model_predict <- lm(predict    ~ 0 + pm02, data = filtered_data)

# Print the slopes for the 0-50 range
cat("Slope for Observed (0-50 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (0-50 range):", coef(model_predict), "\n")

abline(model_sensor,  col = "#1975BA", lwd = 2.5)
abline(model_predict, col = "#BA3E45", lwd = 2.5)

# 7. Legend with larger, bold text
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
# MODIFIED PLOTTING CODE FOR SQUARE PLOT WITHOUT DRIFT-REFERENCE POINTS
###############################################################################
#png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AG_post_600.png", 
#    width = 7, height = 7, units = "in", res = 600)

# Save current graphical parameters
old_par <- par(no.readonly = TRUE)

# Set up square plot with larger text and bold axis titles
par(
  pty      = "s",               # square plot region
  mar      = c(4.5, 4.5, 2, 2), # margins
  cex.axis = 1.5,               # tick-label size
  cex.lab  = 1.8,               # axis-title size
  mgp      = c(3, 1, 0),        # margin lines for labels
  lwd      = 1.5,               # line thickness
  font.lab = 2                  # bold axis titles
)

axis_limit <- c(50, 600)
custom_ticks <- seq(50, 600, by = 100)

# Filter data to 0-50 range for pm02
filtered_data <- merged_data[merged_data$pm02 >= 50 & merged_data$pm02 <= 600, ]

# 1. Scatter: pm02 vs pm02_no.11, with default axes suppressed
plot(
  merged_data$pm02, 
  merged_data$pm02_no.11, 
  xlim   = axis_limit, 
  ylim   = axis_limit, 
  xlab   = "Fidas 200 (µg m⁻³)", 
  ylab   = "AG Field Sensor (µg m⁻³)",
  pch    = 16, 
  col    = "#1975BA",
  asp    = 1,       # 1:1 aspect ratio
  cex    = 1.5,     # point size
  xaxt   = "n",     # suppress x-axis
  yaxt   = "n"      # suppress y-axis
)

# 2. Draw only our custom axes at 100-unit intervals
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)

# 3. Box around the plot
box(lwd = 1.5)

# 4. Overlay calibrated points
points(
  merged_data$pm02, 
  merged_data$predict, 
  pch = 17, 
  col = "#BA3E45",
  cex = 1.5
)

# 5. 1:1 dashed line
abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

# 6. Trend lines through origin - USING FILTERED DATA (0-50 range only)
model_sensor  <- lm(pm02_no.11 ~ 0 + pm02, data = filtered_data)  
model_predict <- lm(predict    ~ 0 + pm02, data = filtered_data)

# Print the slopes for the 0-50 range
cat("Slope for Observed (50-600 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (50-600 range):", coef(model_predict), "\n")

abline(model_sensor,  col = "#1975BA", lwd = 2.5)
abline(model_predict, col = "#BA3E45", lwd = 2.5)

# 7. Legend with larger, bold text
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
# 8) CALCULATE AND SHOW STATISTICS
###############################################################################
calc_stats <- function(df, ref_col = "pm02", pred_col) {
  obs  <- df[[ref_col]]
  pred <- df[[pred_col]]
  N <- length(obs)
  r2 <- summary(lm(pred ~ obs))$r.squared
  rmse <- sqrt(mean((pred - obs)^2, na.rm = TRUE))
  mae <- mean(abs(pred - obs), na.rm = TRUE)
  mbe <- mean(pred - obs, na.rm = TRUE)
  
  # Use openair modStats() to calculate IOA
  # Create a temporary data frame for modStats
  temp_df <- data.frame(obs = obs, mod = pred)
  mod_stats <- modStats(temp_df, obs = "obs", mod = "mod")
  ioa <- mod_stats$IOA
  
  data.frame(
    N    = N,
    R2   = r2,
    RMSE = rmse,
    MAE  = mae,
    MBE  = mbe,
    IOA  = ioa
  )
}

stats_sensor <- calc_stats(merged_data, ref_col = "pm02", pred_col = "pm02_no.11")
stats_calib  <- calc_stats(merged_data, ref_col = "pm02", pred_col = "predict")
stats_drift  <- calc_stats(merged_data, ref_col = "pm02", pred_col = "pm02_refs")

stats_table <- rbind(
  Sensor_Observed          = stats_sensor,
  Calibrated               = stats_calib,
  Drift_Reference_Observed = stats_drift
)

# Round numeric columns to 2 decimals for printing
stats_table_2 <- stats_table
stats_table_2[] <- lapply(stats_table_2, function(x) if(is.numeric(x)) round(x, 2) else x)

stats_table_2

cat("\nPre-calibration R² (pm02_no.11 vs pm02):", round(stats_sensor$R2, 2), "\n")

###############################################################################
# 9) CALCULATE AND SHOW STATISTICS FOR EACH BIN + ALL
###############################################################################
get_stats_table <- function(df, label) {
  stats_sensor <- calc_stats(df, ref_col = "pm02", pred_col = "pm02_no.11")
  stats_calib  <- calc_stats(df, ref_col = "pm02", pred_col = "predict")
  stats_drift  <- calc_stats(df, ref_col = "pm02", pred_col = "pm02_refs")
  tbl <- rbind(
    Sensor_Observed          = stats_sensor,
    Calibrated               = stats_calib,
    Drift_Reference_Observed = stats_drift
  )
  tbl$Subset <- label
  tbl
}

stats_low  <- get_stats_table(split_data[[1]], label = "Low Bin")
stats_high <- get_stats_table(split_data[[2]], label = "High Bin")
stats_all  <- get_stats_table(merged_data,      label = "All Data")

# Combine and reorder columns
stats_combined <- rbind(stats_low, stats_high, stats_all)
stats_combined <- stats_combined[, c("Subset", "N", "R2", "RMSE", "MAE", "MBE", "IOA")]

# Round numeric columns in each table
round_df <- function(df) {
  df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 2) else x)
  df
}
stats_low_2  <- round_df(stats_low)
stats_high_2 <- round_df(stats_high)
stats_all_2  <- round_df(stats_all)
stats_combined_2 <- round_df(stats_combined)

cat("\n## Statistics for Low Bin ##\n")
print(stats_low_2)

cat("\n## Statistics for High Bin ##\n")
print(stats_high_2)

cat("\n## Statistics for All Data (Combined) ##\n")
print(stats_all_2)

cat("\n## Combined Statistics (All Bins + All Data) ##\n")
print(stats_combined_2)

###############################################################################
# 10) CREATE A MARKDOWN TABLE WITHOUT THE 'Drift_Reference_Observed' ROW
###############################################################################
stats_md <- stats_combined_2[!(rownames(stats_combined_2) == "Drift_Reference_Observed"), ]

cat("\n### Markdown Table (Sensor_Observed and Calibrated Only) ###\n\n")
cat("| Subset | Method | N | R2 | RMSE | MAE | MBE | IOA |\n")
cat("|--------|--------|---|----|------|-----|-----|-----|\n")

for (i in rownames(stats_md)) {
  row <- stats_md[i, ]
  cat(
    "|", row$Subset, 
    "|", i, 
    "|", row$N, 
    "|", row$R2, 
    "|", row$RMSE, 
    "|", row$MAE, 
    "|", row$MBE, 
    "|", row$IOA, 
    "|\n"
  )
}