###############################################################################
# 1) SETUP: Load Libraries, Clear Workspace, and Set Config
###############################################################################
library(openair)
library(h2o)
library(httr)
library(dplyr)
# Optional: Set your working directory if desired
setwd("D:/OneDrive - University of Birmingham/Sensor Calibration/Sensor_Calibration/AG/AG_data")
# Clear existing objects
rm(list = ls(all = TRUE))
# Configure HTTP request timeout to 10 minutes
set_config(config(timeout = 6000))
###############################################################################
# 2) READ DATA AND PREPARE
###############################################################################
data <- read.csv("D:/OneDrive - University of Birmingham/Sensor_calibration/merge_AG.csv")
data <- data %>%
  select(-X) %>%
  mutate(
    ID        = row_number(),
    pm02_refs = (pm02_no.33 + pm02_no.34 + pm02_no.63 + pm02_no.64 + pm02_no.65) / 5
  )
pm02_columns <- grep("^pm02_no\\.\\d+", names(data), value = TRUE)
calculate_t4drift_months <- function(column) {
  total_hours <- sum(!is.na(data[[column]]))  
  total_months <- total_hours / 720
  round(total_months, 2)
}
for (col in pm02_columns) {
  sensor_id <- sub("pm02_", "t4drift_", col)
  data[[sensor_id]] <- calculate_t4drift_months(col)
}
target <- "pm02_refs"
predictors <- c("pm02_no.11", "rhum", "atmp")
###############################################################################
# 3) INITIALIZE H2O 
###############################################################################
h2o.init(max_mem_size = "64G")
h2o_data <- as.h2o(data)
###############################################################################
# 4) SPLIT DATA
###############################################################################
split_frames <- h2o.splitFrame(data = h2o_data, ratios = 0.8, seed = 1014)
train_set <- split_frames[[1]]
test_set  <- split_frames[[2]]
###############################################################################
# 5) RUN H2O AUTOML
###############################################################################
automl_models <- h2o.automl(
  x              = predictors,
  y              = target,
  training_frame = train_set,
  max_models     = 6,  
  seed           = 1014
)
###############################################################################
# 6) PREDICT
###############################################################################
best_model <- h2o.get_best_model(automl_models, criterion = "auto")
pred <- h2o.predict(best_model, h2o_data)
pred_df <- as.data.frame(pred)
data <- cbind(data, pred_df)

###############################################################################
# 7) CALCULATE R-SQUARED
###############################################################################
# Function to calculate R-squared
calculate_r2 <- function(observed, predicted) {
  # Remove NA values
  valid_indices <- !is.na(observed) & !is.na(predicted)
  observed <- observed[valid_indices]
  predicted <- predicted[valid_indices]
  
  # Calculate total sum of squares
  SS_tot <- sum((observed - mean(observed))^2)
  
  # Calculate residual sum of squares
  SS_res <- sum((observed - predicted)^2)
  
  # Calculate R-squared
  r2 <- 1 - (SS_res / SS_tot)
  
  return(r2)
}

# Calculate R-squared for pre-calibration (raw data)
r2_pre <- calculate_r2(data$pm02_refs, data$pm02_no.11)

# Calculate R-squared for post-calibration (predicted data)
r2_post <- calculate_r2(data$pm02_refs, data$predict)

# Print R-squared values
cat("Pre-calibration R² =", round(r2_pre, 4), "\n")
cat("Post-calibration R² =", round(r2_post, 4), "\n")

###############################################################################
# 8) PLOT RESULTS
###############################################################################
# Set up PNG device for the scatter plot
png("D:/OneDrive - University of Birmingham/Sensor_calibration/paper/pics/final/AG_step1.png", 
    width = 7, height = 7, units = "in", res = 600)

# Save the current graphical parameters
old_par <- par(no.readonly = TRUE)

# Set up square plot with increased text sizes
par(pty = "s",            # This sets the plot region to be square
    mar = c(4.5, 4.5, 2, 2),  # Increased margins for larger labels
    cex.axis = 1.5,       # Larger axis numbers
    cex.lab = 1.8,        # Larger axis labels
    mgp = c(3, 1, 0),     # Adjust margin line for axis labels
    lwd = 1.5)            # Thicker lines overall

axis_limit <- c(0, 600)

plot(
  data$pm02_refs, data$pm02_no.11, 
  xlim = axis_limit, 
  ylim = axis_limit, 
  xlab = "Drift-reference concentration (µg/m³)", 
  ylab = "AG_no.1 concentration (µg/m³)",
  pch = 16, 
  col = "#335372",
  xaxs = "i",  # 'i' = internal, no axis padding
  yaxs = "i",
  cex = 1.5,   # Larger data points
  asp = 1      # Force 1:1 aspect ratio
)

points(data$pm02_refs, data$predict, 
       pch = 17, 
       col = "#E25659",
       cex = 1.5)  # Larger prediction points

abline(a = 0, b = 1, 
       col = "black", 
       lwd = 3, 
       lty = 2)

legend("topleft",
       legend = c(
         paste("Observed (R² =", round(r2_pre, 3), ")"),
         paste("Calibrated (R² =", round(r2_post, 3), ")")
       ),
       pch = c(16, 17),
       col = c("#335372", "#E25659"),
       bty = "n",         # No box around legend
       cex = 1.8,         # Larger legend text
       pt.cex = 1.8,      # Larger legend symbols
       inset = 0.05)      # Adjust position inside plot

# Close the device to save the file
dev.off()

# Restore original plotting parameters
par(old_par)

# Write out your updated dataset
#write.csv(data, "D:/OneDrive - University of Birmingham/Sensor_calibration/merge_AG_test.csv")