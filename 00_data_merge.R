# Load necessary library
library(dplyr)
library(openair)

rm(list = ls(all = TRUE))
# Set the working directory to where the CSV files are stored
setwd("D:/OneDrive - University of Birmingham/Sensor Calibration/Sensor_Calibration/AG_data")  # Replace with your folder path

# List all relevant CSV files
file_list <- list.files(pattern = "sensor_no\\.[0-9]+_20231201_20241031\\.csv")

# Initialize an empty list to store data frames
df_list <- list()

# Loop through each file
for (file in file_list) {
  # Extract sensor number from the filename
  sensor_no <- sub("sensor_no\\.(\\d+)_.*", "\\1", file)
  
  # Read the CSV file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Keep only the columns: date, pm02, atmp, rhum
  df <- df %>% select(date, pm02, atmp, rhum)
  
  # Rename the columns (except 'date')
  colnames(df)[-1] <- paste0(colnames(df)[-1], "_no.", sensor_no)
  
  # Add the data frame to the list
  df_list[[file]] <- df
}

# Merge all data frames by 'date'
merged_df <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), df_list)
# View the first few rows of the merged dataframe
head(merged_df)

# --- Now, merge with fidas.csv ---

# Read the fidas.csv file
fidas_df <- read.csv("fidas.csv", stringsAsFactors = FALSE)
fidas_df <- fidas_df %>% select(date, pm02, atmp, rhum)

merged_df$date <- as.POSIXct(merged_df$date, format = "%Y-%m-%d %H:%M")
hourly_merged_df <- timeAverage(merged_df, avg.time = "hour", statistic = "mean", na.rm = TRUE)

fidas_df$date <- as.POSIXct(fidas_df$date, format = "%d/%m/%Y %H:%M")
hourly_fidas_df <- timeAverage(fidas_df, avg.time = "hour", statistic = "mean", na.rm = TRUE)
hourly_fidas_df_shifted <- hourly_fidas_df
hourly_fidas_df_shifted[, -1] <- dplyr::lead(hourly_fidas_df[, -1], n = 1)

# Merge fidas data with the merged sensor data
final_merged_df <- merge(hourly_merged_df, hourly_fidas_df_shifted, by = "date", all = TRUE)

# Remove rows where any pm02 column is NA
final_merged_df <- final_merged_df %>% filter(if_all(starts_with("pm02"), ~ !is.na(.)))
# Filter out rows where pm02 is greater than 1000
filtered_df <- final_merged_df %>% 
  dplyr::filter(pm02 <= 1000)

write.csv(filtered_df, "D:/OneDrive - University of Birmingham/Sensor Calibration/Sensor_Calibration/AG_data/merge_AG.csv")
#
plot(filtered_df$date, filtered_df$pm02_no.11, type = "l", ylim = c(0, 1500))
points(filtered_df$date, filtered_df$pm02, type = "l", col = "red")

plot(filtered_df$pm02, filtered_df$pm02_no.113, ylim =c(0, 1500), xlim =c(0, 1500))

