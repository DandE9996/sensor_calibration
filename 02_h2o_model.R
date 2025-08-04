#install.packages('h2o')

###############################################################################
# 1) SETUP: Load Libraries, Clear Workspace, and Set Config
###############################################################################
library(openair)
library(h2o)
library(httr)
library(dplyr)
library(viridis)
library(fields)
library(scales)
library(knitr)

rm(list = ls(all = TRUE))
set_config(config(timeout = 6000))

###############################################################################
# 2) READ DATA AND PREPARE
###############################################################################
data <- read.csv("D:/OneDrive - University of Birmingham/Sensor_calibration/merge_AG.csv")

data <- data %>%
  select(-date, -X) %>%
  mutate(
    ID        = row_number(),
    pm02_refs = (pm02_no.33 + pm02_no.34 + pm02_no.63 + pm02_no.64 + pm02_no.65) / 5
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

split_data <- split_data_by_pm02(data, thresholds = c(50))

###############################################################################
# 4) INITIALIZE H2O AND CREATE MODEL SAVE DIRECTORIES
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
# 5.1) EXTRACT AND DISPLAY AUTOML MODEL PERFORMANCE
###############################################################################
# Create a list to store all models performance
all_models_performance_list <- list()

# Extract performance metrics for ALL models in each AutoML run
for (i in seq_along(automl_models)) {
  # Get the full leaderboard
  lb <- as.data.frame(automl_models[[i]]@leaderboard)
  
  # Add subset information
  lb$Model_Subset <- ifelse(i == 1, "Low (<50 µg/m³)", "High (≥50 µg/m³)")
  lb$Rank <- 1:nrow(lb)
  
  # Get detailed performance for each model
  model_details_list <- list()
  
  for (j in 1:nrow(lb)) {
    model_id <- lb$model_id[j]
    model <- h2o.getModel(model_id)
    
    # Get performance on training and test sets
    train_perf <- h2o.performance(model, train_sets[[i]])
    test_perf <- h2o.performance(model, test_sets[[i]])
    
    # Extract algorithm type
    algorithm <- model@algorithm
    
    # Create detailed performance data frame
    model_details <- data.frame(
      Model_Subset = lb$Model_Subset[j],
      Rank = lb$Rank[j],
      Model_ID = model_id,
      Algorithm = algorithm,
      Train_RMSE = h2o.rmse(train_perf),
      Train_MAE = h2o.mae(train_perf),
      Train_R2 = h2o.r2(train_perf),
      Test_RMSE = h2o.rmse(test_perf),
      Test_MAE = h2o.mae(test_perf),
      Test_R2 = h2o.r2(test_perf),
      stringsAsFactors = FALSE
    )
    
    model_details_list[[j]] <- model_details
  }
  
  # Combine all models for this subset
  subset_performance <- do.call(rbind, model_details_list)
  all_models_performance_list[[i]] <- subset_performance
}

# Combine all models from all subsets
all_models_performance_df <- do.call(rbind, all_models_performance_list)

# Round numeric columns
all_models_performance_df <- all_models_performance_df %>%
  mutate(across(where(is.numeric), round, 3))

# Display the complete AutoML model performance table
cat("\n## Complete AutoML Model Performance Table ##\n")
print(all_models_performance_df)

# Save the complete AutoML model performance table to CSV
write.csv(all_models_performance_df, 
          file = "D:/OneDrive - University of Birmingham/Sensor_calibration/automl_all_models_performance.csv",
          row.names = FALSE)
cat("\nComplete AutoML performance table saved to: D:/OneDrive - University of Birmingham/Sensor_calibration/automl_all_models_performance.csv\n")

# Also create a summary table with just the best models
best_models_df <- all_models_performance_df %>%
  filter(Rank == 1)

cat("\n## Best Models Summary ##\n")
print(best_models_df)

# Save best models summary
write.csv(best_models_df, 
          file = "D:/OneDrive - University of Birmingham/Sensor_calibration/automl_best_models_summary.csv",
          row.names = FALSE)
cat("\nBest models summary saved to: D:/OneDrive - University of Birmingham/Sensor_calibration/automl_best_models_summary.csv\n")

# Create a nice formatted table using kable
kable(
  all_models_performance_df,
  caption = "Complete H2O AutoML Model Performance Metrics",
  align = "c"
)

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
save_folder <- "D:/OneDrive - University of Birmingham/Sensor_calibration/models"
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
# 8) PERFORMANCE METRICS FUNCTIONS
###############################################################################
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
  
  data.frame(N=length(obs), R2=R2, RMSE=RMSE, MAE=MAE, MBE=MBE, IOA=IOA)
}

###############################################################################
# 9) CALCULATE PRE- AND POST-CALIBRATION METRICS
###############################################################################
# A) POST-CALIBRATION: pred = 'predict'
stats_low_post  <- calc_stats(split_data[[1]]$pm02, split_data[[1]]$predict)
stats_high_post <- calc_stats(split_data[[2]]$pm02, split_data[[2]]$predict)
stats_all_post  <- calc_stats(merged_data$pm02,     merged_data$predict)

df_stats_post <- rbind(
  cbind(Subset = "Low (<50 µg/m³)",  stats_low_post),
  cbind(Subset = "High (≥50 µg/m³)", stats_high_post),
  cbind(Subset = "All Data",         stats_all_post)
)

# B) PRE-CALIBRATION: pred = 'pm02_refs'
stats_low_pre  <- calc_stats(split_data[[1]]$pm02, split_data[[1]]$pm02_refs)
stats_high_pre <- calc_stats(split_data[[2]]$pm02, split_data[[2]]$pm02_refs)
stats_all_pre  <- calc_stats(merged_data$pm02,     merged_data$pm02_refs)

df_stats_pre <- rbind(
  cbind(Subset = "Low (<50 µg/m³)",  stats_low_pre),
  cbind(Subset = "High (≥50 µg/m³)", stats_high_pre),
  cbind(Subset = "All Data",         stats_all_pre)
)

###############################################################################
# 10) MERGE BOTH TABLES SIDE-BY-SIDE (PRE vs POST)
###############################################################################
df_stats_pre_ren  <- df_stats_pre %>% 
  rename_with(~ paste0(., "_pre"), -Subset)
df_stats_post_ren <- df_stats_post %>% 
  rename_with(~ paste0(., "_post"), -Subset)

df_stats_merged <- merge(df_stats_pre_ren, df_stats_post_ren, by = "Subset") %>%
  select(
    Subset,
    N_pre,   R2_pre,   RMSE_pre,   MAE_pre,   MBE_pre,   IOA_pre,
    N_post,  R2_post,  RMSE_post,  MAE_post,  MBE_post,  IOA_post
  ) %>%
  mutate(across(-Subset, round, 2))

###############################################################################
# 11) DISPLAY THE FINAL TABLE
###############################################################################
cat("\n## Comparison: PRE vs POST Calibration (Rounded to 2 decimals) ##\n")
print(df_stats_merged)

kable(
  df_stats_merged,
  caption = "Comparison: Pre-Calibration (pm02_refs) vs Post-Calibration (predict)",
  align   = "c"
)

###############################################################################
# 12) FIGURE - SCATTER PLOT (100-unit ticks from 50 to 600)
###############################################################################
#png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AG_ref_calibration_50.png", 
#    width = 7, height = 7, units = "in", res = 600)

# Save original parameters
old_par <- par(no.readonly = TRUE)

# Square plot, larger text
par(
  pty      = "s",
  mar      = c(4.5, 4.5, 2, 2),
  cex.axis = 1.5,
  cex.lab  = 1.8,
  mgp      = c(3, 1, 0),
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
  xlim   = axis_limit, ylim = axis_limit,
  xlab   = "Fidas 200 (µg m⁻³)", 
  ylab   = "AG Drift-Reference (µg m⁻³)",
  pch    = 16, col = "#1975BA", cex = 1.5,
  asp    = 1,
  xaxt   = "n",
  yaxt   = "n"
)

# Custom axes at 100-unit intervals
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)

# Draw box around plot
box(lwd = 1.5)

# Overlay calibrated predictions
points(
  merged_data$pm02, merged_data$predict,
  pch = 17, col = "#F47F72", cex = 1.5
)

# 1:1 dashed line
abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

# Trend lines forced through origin using FILTERED data (0-50 range only)
model_sensor  <- lm(pm02_refs ~ 0 + pm02, data = filtered_data)
model_predict <- lm(predict   ~ 0 + pm02, data = filtered_data)

# Print the slopes for the 0-50 range
cat("Slope for Observed (0-50 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (0-50 range):", coef(model_predict), "\n")

abline(a = 0, b = coef(model_sensor),  col = "#1975BA", lwd = 2.5)
abline(a = 0, b = coef(model_predict), col = "#F47F72", lwd = 2.5)

# Legend
legend(
  "topleft",
  legend = c("Raw", "Calibrated"),
  col    = c("#1975BA", "#F47F72"),
  pch    = c(16, 17),
  bty    = "n",
  cex    = 1.6,
  pt.cex = 1.6
)

dev.off()

# Restore original plotting parameters if needed
# par(old_par)

###############################################################################
# 12) FIGURE - SCATTER PLOT (100-unit ticks from 50 to 600)
###############################################################################
#png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AG_ref_calibration_600.png", 
#    width = 7, height = 7, units = "in", res = 600)

# Save original parameters
old_par <- par(no.readonly = TRUE)

# Square plot, larger text
par(
  pty      = "s",
  mar      = c(4.5, 4.5, 2, 2),
  cex.axis = 1.5,
  cex.lab  = 1.8,
  mgp      = c(3, 1, 0),
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
  xlim   = axis_limit, ylim = axis_limit,
  xlab   = "Fidas 200 (µg m⁻³)", 
  ylab   = "AG Drift-Reference (µg m⁻³)",
  pch    = 16, col = "#1975BA", cex = 1.5,
  asp    = 1,
  xaxt   = "n",
  yaxt   = "n"
)

# Custom axes at 100-unit intervals
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)

# Draw box around plot
box(lwd = 1.5)

# Overlay calibrated predictions
points(
  merged_data$pm02, merged_data$predict,
  pch = 17, col = "#F47F72", cex = 1.5
)

# 1:1 dashed line
abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

# Trend lines forced through origin using FILTERED data (0-50 range only)
model_sensor  <- lm(pm02_refs ~ 0 + pm02, data = filtered_data)
model_predict <- lm(predict   ~ 0 + pm02, data = filtered_data)

# Print the slopes for the 0-50 range
cat("Slope for Observed (50-600 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (50-600 range):", coef(model_predict), "\n")

abline(a = 0, b = coef(model_sensor),  col = "#1975BA", lwd = 2.5)
abline(a = 0, b = coef(model_predict), col = "#F47F72", lwd = 2.5)

# Legend
legend(
  "topleft",
  legend = c("Raw", "Calibrated"),
  col    = c("#1975BA", "#F47F72"),
  pch    = c(16, 17),
  bty    = "n",
  cex    = 1.6,
  pt.cex = 1.6
)

dev.off()

# Restore original plotting parameters if needed
# par(old_par)

###############################################################################
# 13) FIGURE - TIME SERIES PLOT
###############################################################################
par(mar = c(4.5, 4.5, 3, 2), cex.axis = 1.5, cex.lab = 1.8, cex.main = 2.0, mgp = c(3, 1, 0), lwd = 1.5)
axis_limit_ts <- c(0, 750)

plot(merged_data$ID, merged_data$pm02, type = "l", lwd = 2.5, xlim = range(merged_data$ID),
     ylim = axis_limit_ts, xlab = "Timeseries", ylab = "PM2.5 (µg m⁻³)",
     main = "Time Series: Fidas vs Sensor Calibration", col = "#1975BA")

lines(merged_data$ID, merged_data$predict, col = "#F47F72", lwd = 2.5)
lines(merged_data$ID, merged_data$pm02_refs, col = "black", lwd = 2.5, lty = 2)

legend("topright",
       legend = c("Fidas (pm02)", "Post-calibration", "Pre-Calibration"),
       col    = c("#1975BA", "#F47F72", "black"),
       lty    = c(1, 1, 2),
       lwd    = 2.5,
       bty    = "n",
       cex    = 1.8)

grid(lwd = 1.5)
par(old_par)