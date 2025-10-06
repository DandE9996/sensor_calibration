# install.packages("h2o")

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
library(stringr)

rm(list = ls(all = TRUE))
set_config(config(timeout = 6000))

h2o.no_progress()  # cleaner console

###############################################################################
# 2) READ DATA AND PREPARE
###############################################################################
data <- read.csv("D:/OneDrive - University of Birmingham/Sensor_calibration/merge_AG.csv")

data <- data %>%
  # keep date if you need it later. Removing per original code.
  select(-X) %>%
  mutate(
    ID        = row_number(),
    pm02_refs = (pm02_no.33 + pm02_no.34 + pm02_no.63 + pm02_no.64 + pm02_no.65) / 5
  )

# Sanity checks
stopifnot("pm02" %in% colnames(data))
stopifnot(is.numeric(data$pm02))

target <- "pm02"
predictors <- c("pm02_refs", "rhum", "atmp")
predictors <- intersect(predictors, colnames(data)) # in case some columns are absent
stopifnot(length(predictors) > 0)

###############################################################################
# 3) DEFINE FUNCTION TO SPLIT DATA BY pm02 THRESHOLD
###############################################################################
split_data_by_pm02 <- function(data, thresholds) {
  n <- length(thresholds) + 1
  split_list <- vector("list", n)
  for (i in seq_len(n)) {
    if (i == 1) {
      split_list[[i]] <- data %>% dplyr::filter(pm02_refs < thresholds[1])
    } else if (i == n) {
      split_list[[i]] <- data %>% dplyr::filter(pm02_refs >= thresholds[n - 1])
    } else {
      split_list[[i]] <- data %>%
        dplyr::filter(pm02_refs >= thresholds[i - 1] & pm02_refs < thresholds[i])
    }
  }
  split_list
}

split_data <- split_data_by_pm02(data, thresholds = c(50))

###############################################################################
# 4) INITIALIZE H2O AND CREATE TRAIN TEST SPLITS
###############################################################################
h2o.init(max_mem_size = "256G")

h2o_splits <- lapply(split_data, as.h2o)
split_frames <- lapply(h2o_splits, function(df) {
  h2o.splitFrame(data = df, ratios = 0.8, seed = 1014)
})
train_sets <- lapply(split_frames, `[[`, 1)
test_sets  <- lapply(split_frames, `[[`, 2)

###############################################################################
# 5) RUN H2O AUTOML (REGRESSION) FOR EACH SUBSET
###############################################################################
automl_models <- vector("list", length(train_sets))

for (i in seq_along(train_sets)) {
  automl_models[[i]] <- h2o.automl(
    x               = predictors,
    y               = target,
    training_frame  = train_sets[[i]],
    max_models      = 30,     # request 30 models
    seed            = 1014,
    sort_metric     = "RMSE"
  )
}

###############################################################################
# 5.1) EXTRACT AND SAVE PERFORMANCE FOR ALL MODELS
###############################################################################
# Helper: safe device close
safe_dev_off <- function() {
  devs <- dev.list()
  if (!is.null(devs)) dev.off()
}

# Subset labels
subset_labels <- c("Low (<50 µg/m³)", "High (≥50 µg/m³)")
subset_labels <- subset_labels[seq_along(automl_models)]

# Collect performance tables from all subsets
all_models_performance_list <- list()

for (i in seq_along(automl_models)) {
  aml <- automl_models[[i]]
  
  # Get leaderboard with extra columns
  lb_h2o <- h2o.get_leaderboard(aml, extra_columns = "ALL")
  lb <- as.data.frame(lb_h2o)
  
  # Guarantee character model ids
  lb$model_id <- as.character(lb$model_id)
  
  # Add rank column based on current order
  lb$LB_Rank <- seq_len(nrow(lb))
  
  # Prefix leaderboard columns except model_id
  lb_prefixed <- lb %>%
    rename(Model_ID = model_id) %>%
    rename_with(~ paste0("LB_", .x), -Model_ID)
  
  # Compute train and test performance for every model in this leaderboard
  model_details_list <- vector("list", nrow(lb_prefixed))
  
  for (j in seq_len(nrow(lb_prefixed))) {
    model_id <- lb_prefixed$Model_ID[j]
    model <- h2o.getModel(model_id)
    
    # Performances
    train_perf <- h2o.performance(model, train_sets[[i]])
    test_perf  <- h2o.performance(model,  test_sets[[i]])
    
    # Algorithm type
    algorithm <- model@algorithm
    
    # Build row
    model_details_list[[j]] <- data.frame(
      Model_Subset = subset_labels[i],
      Model_ID     = model_id,
      Algorithm    = algorithm,
      Train_RMSE   = as.numeric(h2o.rmse(train_perf)),
      Train_MAE    = as.numeric(h2o.mae(train_perf)),
      Train_R2     = as.numeric(h2o.r2(train_perf)),
      Test_RMSE    = as.numeric(h2o.rmse(test_perf)),
      Test_MAE     = as.numeric(h2o.mae(test_perf)),
      Test_R2      = as.numeric(h2o.r2(test_perf)),
      stringsAsFactors = FALSE
    )
  }
  
  subset_perf <- dplyr::bind_rows(model_details_list)
  
  # Join leaderboard metrics to the performance table
  subset_perf_full <- subset_perf %>%
    left_join(lb_prefixed, by = "Model_ID")
  
  # Round numeric columns to 3 decimals for readability
  subset_perf_full <- subset_perf_full %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  all_models_performance_list[[i]] <- subset_perf_full
}

# Combine all subsets
all_models_performance_df <- dplyr::bind_rows(all_models_performance_list)

# Reorder columns for clarity
# Put key identifiers first, then test metrics, then train metrics, then leaderboard metrics
id_cols   <- c("Model_Subset", "Model_ID", "Algorithm")
test_cols <- c("Test_RMSE", "Test_MAE", "Test_R2")
train_cols <- c("Train_RMSE", "Train_MAE", "Train_R2")
lb_cols   <- setdiff(colnames(all_models_performance_df),
                     c(id_cols, test_cols, train_cols))
all_models_performance_df <- all_models_performance_df[, c(id_cols, test_cols, train_cols, lb_cols)]

# Write the complete table to CSV
out_csv_all <- "D:/OneDrive - University of Birmingham/Sensor_calibration/automl_all_models_performance.csv"
write.csv(all_models_performance_df, file = out_csv_all, row.names = FALSE)
cat("\nComplete AutoML performance table saved to:\n", out_csv_all, "\n")

# Also save a best models summary per subset based on leaderboard rank
best_models_df <- all_models_performance_df %>%
  group_by(Model_Subset) %>%
  arrange(LB_LB_Rank, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

out_csv_best <- "D:/OneDrive - University of Birmingham/Sensor_calibration/automl_best_models_summary.csv"
write.csv(best_models_df, file = out_csv_best, row.names = FALSE)
cat("\nBest models summary saved to:\n", out_csv_best, "\n")

# Pretty print in session
kable(
  all_models_performance_df,
  caption = "Complete H2O AutoML Model Performance Metrics (Train and Test, plus Leaderboard)"
)

###############################################################################
# 6) PREDICT USING THE BEST MODELS ON EACH SUBSET
###############################################################################
for (i in seq_along(split_data)) {
  h2o_data   <- as.h2o(split_data[[i]])
  best_model <- automl_models[[i]]@leader  # R access to the best model
  pred       <- h2o.predict(best_model, h2o_data)
  pred_df    <- as.data.frame(pred)
  split_data[[i]] <- cbind(split_data[[i]], predict = pred_df$predict)
}

###############################################################################
# 7) MERGE ALL SUBSETS BACK AND SORT BY ID
###############################################################################
merged_data <- do.call(rbind, split_data) %>% arrange(ID)

###############################################################################
# 8) SAVE THE TWO BEST MODELS TO DISK
###############################################################################
save_folder <- "D:/OneDrive - University of Birmingham/Sensor_calibration/models"
dir.create(save_folder, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(automl_models)) {
  best_model <- automl_models[[i]]@leader
  model_path <- h2o.saveModel(object = best_model, path = save_folder, force = TRUE)
  cat(sprintf("Best model for subset %s saved at: %s\n", subset_labels[i], model_path))
}

###############################################################################
# 9) PERFORMANCE METRICS FUNCTIONS
###############################################################################
calc_stats <- function(obs, pred) {
  valid_idx <- complete.cases(obs, pred)
  obs  <- obs[valid_idx]
  pred <- pred[valid_idx]
  
  R2   <- cor(obs, pred)^2
  RMSE <- sqrt(mean((obs - pred)^2))
  MAE  <- mean(abs(obs - pred))
  MBE  <- mean(obs - pred)
  
  temp_df <- data.frame(obs = obs, mod = pred)
  mod_stats <- modStats(temp_df, obs = "obs", mod = "mod")
  IOA <- mod_stats$IOA
  
  data.frame(N = length(obs), R2 = R2, RMSE = RMSE, MAE = MAE, MBE = MBE, IOA = IOA)
}

###############################################################################
# 10) CALCULATE PRE AND POST CALIBRATION METRICS
###############################################################################
stats_low_post  <- calc_stats(split_data[[1]]$pm02, split_data[[1]]$predict)
stats_high_post <- calc_stats(split_data[[2]]$pm02, split_data[[2]]$predict)
stats_all_post  <- calc_stats(merged_data$pm02,     merged_data$predict)

df_stats_post <- rbind(
  cbind(Subset = "Low (<50 µg/m³)",  stats_low_post),
  cbind(Subset = "High (≥50 µg/m³)", stats_high_post),
  cbind(Subset = "All Data",         stats_all_post)
)

stats_low_pre  <- calc_stats(split_data[[1]]$pm02, split_data[[1]]$pm02_refs)
stats_high_pre <- calc_stats(split_data[[2]]$pm02, split_data[[2]]$pm02_refs)
stats_all_pre  <- calc_stats(merged_data$pm02,     merged_data$pm02_refs)

df_stats_pre <- rbind(
  cbind(Subset = "Low (<50 µg/m³)",  stats_low_pre),
  cbind(Subset = "High (≥50 µg/m³)", stats_high_pre),
  cbind(Subset = "All Data",         stats_all_pre)
)

###############################################################################
# 11) MERGE BOTH TABLES SIDE BY SIDE
###############################################################################
df_stats_pre_ren  <- df_stats_pre %>% rename_with(~ paste0(., "_pre"),  -Subset)
df_stats_post_ren <- df_stats_post %>% rename_with(~ paste0(., "_post"), -Subset)

df_stats_merged <- merge(df_stats_pre_ren, df_stats_post_ren, by = "Subset") %>%
  select(
    Subset,
    N_pre,   R2_pre,   RMSE_pre,   MAE_pre,   MBE_pre,   IOA_pre,
    N_post,  R2_post,  RMSE_post,  MAE_post,  MBE_post,  IOA_post
  ) %>%
  mutate(across(-Subset, round, 2))

cat("\n## Comparison: PRE vs POST Calibration (Rounded to 2 decimals) ##\n")
print(df_stats_merged)

kable(
  df_stats_merged,
  caption = "Comparison: Pre-Calibration (pm02_refs) vs Post-Calibration (predict)",
  align   = "c"
)

###############################################################################
# 12) FIGURE - SCATTER PLOT 0–50
###############################################################################
# png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AG_ref_calibration_50.png",
#     width = 7, height = 7, units = "in", res = 600)

old_par <- par(no.readonly = TRUE)
par(pty = "s", mar = c(4.5, 4.5, 2, 2), cex.axis = 1.5, cex.lab = 1.8, mgp = c(3, 1, 0), lwd = 1.5, font.lab = 2)

axis_limit   <- c(0, 50)
custom_ticks <- seq(0, 50, by = 10)

filtered_data <- merged_data[merged_data$pm02 >= 0 & merged_data$pm02 <= 50, ]

plot(
  merged_data$pm02, merged_data$pm02_refs,
  xlim = axis_limit, ylim = axis_limit,
  xlab = "Fidas 200 (µg m⁻³)",
  ylab = "AG Drift-Reference (µg m⁻³)",
  pch = 16, col = "#1975BA", cex = 1.5,
  asp = 1, xaxt = "n", yaxt = "n"
)
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
box(lwd = 1.5)

points(merged_data$pm02, merged_data$predict, pch = 17, col = "#F47F72", cex = 1.5)

abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

model_sensor  <- lm(pm02_refs ~ 0 + pm02, data = filtered_data)
model_predict <- lm(predict   ~ 0 + pm02, data = filtered_data)

cat("Slope for Observed (0-50 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (0-50 range):", coef(model_predict), "\n")

abline(a = 0, b = coef(model_sensor),  col = "#1975BA", lwd = 2.5)
abline(a = 0, b = coef(model_predict), col = "#F47F72", lwd = 2.5)

legend(
  "topleft",
  legend = c("Raw", "Calibrated"),
  col    = c("#1975BA", "#F47F72"),
  pch    = c(16, 17),
  bty    = "n",
  cex    = 1.6,
  pt.cex = 1.6
)

safe_dev_off()
par(old_par)

###############################################################################
# 13) FIGURE - SCATTER PLOT 50–600
###############################################################################
# png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AG_ref_calibration_600.png",
#     width = 7, height = 7, units = "in", res = 600)

old_par <- par(no.readonly = TRUE)
par(pty = "s", mar = c(4.5, 4.5, 2, 2), cex.axis = 1.5, cex.lab = 1.8, mgp = c(3, 1, 0), lwd = 1.5, font.lab = 2)

axis_limit   <- c(50, 600)
custom_ticks <- seq(50, 600, by = 100)

filtered_data <- merged_data[merged_data$pm02 >= 50 & merged_data$pm02 <= 600, ]

plot(
  merged_data$pm02, merged_data$pm02_refs,
  xlim = axis_limit, ylim = axis_limit,
  xlab = "Fidas 200 (µg m⁻³)",
  ylab = "AG Drift-Reference (µg m⁻³)",
  pch = 16, col = "#1975BA", cex = 1.5,
  asp = 1, xaxt = "n", yaxt = "n"
)
axis(1, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
axis(2, at = custom_ticks, labels = TRUE, cex.axis = 1.5)
box(lwd = 1.5)

points(merged_data$pm02, merged_data$predict, pch = 17, col = "#F47F72", cex = 1.5)

abline(a = 0, b = 1, col = "black", lwd = 3, lty = 2)

model_sensor  <- lm(pm02_refs ~ 0 + pm02, data = filtered_data)
model_predict <- lm(predict   ~ 0 + pm02, data = filtered_data)

cat("Slope for Observed (50-600 range):", coef(model_sensor), "\n")
cat("Slope for Calibrated (50-600 range):", coef(model_predict), "\n")

abline(a = 0, b = coef(model_sensor),  col = "#1975BA", lwd = 2.5)
abline(a = 0, b = coef(model_predict), col = "#F47F72", lwd = 2.5)

legend(
  "topleft",
  legend = c("Raw", "Calibrated"),
  col    = c("#1975BA", "#F47F72"),
  pch    = c(16, 17),
  bty    = "n",
  cex    = 1.6,
  pt.cex = 1.6
)

safe_dev_off()
par(old_par)

###############################################################################
# 14) FIGURE - TIME SERIES PLOT
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
