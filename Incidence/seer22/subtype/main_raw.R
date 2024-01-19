

# Load the necessary libraries
library(dplyr)

# Load the data from the GitHub repository
file_url <- "https://raw.githubusercontent.com/filhoalm/Breast_cancer/main/Incidence/seer22/subtype/seer22_race_subtype.csv"
breast_cancer_data <- read.csv(file_url, stringsAsFactors = FALSE)

# Rename the variables to the desired names
names(breast_cancer_data) <- c("age", "year", "race", "subtype", "rate", "cases", "py")

# Convert 'year' to numeric and adjust the actual year
breast_cancer_data$year <- as.numeric(breast_cancer_data$year) + 1999

# Create an identifier variable for reshaping
breast_cancer_data <- breast_cancer_data %>%
  mutate(id = paste(age, race, subtype, sep = "_"))

# Reshape 'cases' and 'py' to wide format
bc_cases_wide <- reshape(breast_cancer_data, idvar = "id", timevar = "year", direction = "wide", v.names = "cases")
bc_py_wide <- reshape(breast_cancer_data, idvar = "id", timevar = "year", direction = "wide", v.names = "py")

# Merge the 'cases' and 'py' data frames
merged_data <- merge(bc_cases_wide, bc_py_wide, by = "id", all = TRUE)

# Create a sorted vector of year column names for cases and py
years <- sort(unique(breast_cancer_data$year))
sorted_colnames <- unlist(lapply(years, function(y) c(paste0("cases.", y), paste0("py.", y))))

# Select columns in the order of 'id', then alternating 'cases.YEAR' and 'py.YEAR'
final_data <- merged_data %>%
  select(c(id, sorted_colnames))

# Split the 'id' back into 'age', 'race', and 'subtype' and convert them accordingly
final_data <- final_data %>%
  mutate(age = as.numeric(unlist(lapply(strsplit(id, "_"), `[`, 1))),
         race = factor(unlist(lapply(strsplit(id, "_"), `[`, 2)), 
                       levels = c("0", "1", "2", "3", "4", "5"),
                       labels = c("NHW", "NHB", "AIAN", "API", "HIS", "Unknown")),
         subtype = factor(unlist(lapply(strsplit(id, "_"), `[`, 3)),
                          levels = c("0", "1", "2", "3", "4", "5"),
                          labels = c("HR+/HER2+", "HR-/HER2+", "HR+/HER2-", "HR-/HER2-", "Unknown", "Recode not availab")),
         .keep = "unused")

#head

# Exclude the 'id' variable
final_data$id <- NULL

# Reorder columns so 'age', 'race', and 'er' are the first columns
final_data <- final_data %>%
  select(age, race, subtype, everything())


{
df1 <- subset(df, race == "NHW" & subtype == "HR+/HER2+")
df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
df2 <- df2a[-(22:21)] 
# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: NHW - HR+/HER2+"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
combined_data_nhw_positive <- rbind(summary_info, df2)

# View the combined data
head(combined_data_nhw_positive)
#combined_data_nhw_positive

# For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
write.csv(combined_data_nhw_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/nhw_HR+_HER2+.csv", row.names = FALSE)

df1 <- subset(df, race == "NHW" & subtype == "HR+/HER2-")
df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
df2 <- df2a[-(22:21)] 
# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: NHW - HR+/HER2-"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
combined_data_nhw_positive <- rbind(summary_info, df2)

# View the combined data
head(combined_data_nhw_positive)
#combined_data_nhw_positive

# For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
write.csv(combined_data_nhw_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/nhw_HR+_HER2-.csv", row.names = FALSE)

df1 <- subset(df, race == "NHW" & subtype == "HR-/HER2+")
df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
df2 <- df2a[-(22:21)] 
# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: NHW - HR-/HER2+"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
combined_data_nhw_positive <- rbind(summary_info, df2)

# View the combined data
head(combined_data_nhw_positive)
#combined_data_nhw_positive

# For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
write.csv(combined_data_nhw_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/nhw_HR-_HER2+.csv", row.names = FALSE)

df1 <- subset(df, race == "NHW" & subtype == "HR-/HER2-")
df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
df2 <- df2a[-(22:21)] 
# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: NHW - HR-/HER2-"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
combined_data_nhw_positive <- rbind(summary_info, df2)

# View the combined data
head(combined_data_nhw_positive)
#combined_data_nhw_positive

# For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
write.csv(combined_data_nhw_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/nhw_HR-_HER2-.csv", row.names = FALSE)
} #NHW

{
  df1 <- subset(df, race == "NHB" & subtype == "HR+/HER2+")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: NHB - HR+/HER2+"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_nhb_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_nhb_positive)
  #combined_data_nhb_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_nhb_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/nhb_HR+_HER2+.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "NHB" & subtype == "HR+/HER2-")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: NHB - HR+/HER2-"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_nhb_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_nhb_positive)
  #combined_data_nhb_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_nhb_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/nhb_HR+_HER2-.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "NHB" & subtype == "HR-/HER2+")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: NHB - HR-/HER2+"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_nhb_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_nhb_positive)
  #combined_data_nhb_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_nhb_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/nhb_HR-_HER2+.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "NHB" & subtype == "HR-/HER2-")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: NHB - HR-/HER2-"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_nhb_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_nhb_positive)
  #combined_data_nhb_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_nhb_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/nhb_HR-_HER2-.csv", row.names = FALSE)
} #NHB

{
  df1 <- subset(df, race == "HIS" & subtype == "HR+/HER2+")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: HIS - HR+/HER2+"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_his_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_his_positive)
  #combined_data_his_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_his_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/his_HR+_HER2+.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "HIS" & subtype == "HR+/HER2-")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: HIS - HR+/HER2-"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_his_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_his_positive)
  #combined_data_his_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_his_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/his_HR+_HER2-.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "HIS" & subtype == "HR-/HER2+")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: HIS - HR-/HER2+"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_his_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_his_positive)
  #combined_data_his_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_his_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/his_HR-_HER2+.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "HIS" & subtype == "HR-/HER2-")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: HIS - HR-/HER2-"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_his_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_his_positive)
  #combined_data_his_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_his_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/his_HR-_HER2-.csv", row.names = FALSE)
} #HIS

{
  df1 <- subset(df, race == "API" & subtype == "HR+/HER2+")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: API - HR+/HER2+"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_api_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_api_positive)
  #combined_data_api_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_api_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/api_HR+_HER2+.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "API" & subtype == "HR+/HER2-")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: API - HR+/HER2-"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_api_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_api_positive)
  #combined_data_api_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_api_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/api_HR+_HER2-.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "API" & subtype == "HR-/HER2+")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: API - HR-/HER2+"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_api_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_api_positive)
  #combined_data_api_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_api_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/api_HR-_HER2+.csv", row.names = FALSE)
  
  df1 <- subset(df, race == "API" & subtype == "HR-/HER2-")
  df2a <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed
  df2 <- df2a[-(22:21)] 
  # Create a dataframe with a single row and the same number of columns as df2
  summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
  colnames(summary_info) <- colnames(df2)
  
  # Fill in the summary information
  summary_info[1, 1] <- "Title: Breast Cancer"
  summary_info[2, 1] <- "Description: API - HR-/HER2-"
  summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
  summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
  summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available
  
  # Combine the summary information with the data
  combined_data_api_positive <- rbind(summary_info, df2)
  
  # View the combined data
  head(combined_data_api_positive)
  #combined_data_api_positive
  
  # For each race anr ethnicity save the combined data to a CSV file, modifying the filename as needed
  write.csv(combined_data_api_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/api_HR-_HER2-.csv", row.names = FALSE)
} #API

###

# Explore webtool functions

my_path <- "C:/Users/filhoam/Desktop/NIH/RCode 8-16-22/RCode 8-16-22/APC/"
setwd(my_path)  
functions <- list.files(pattern = "*.R")       # Get all file names
apc<- lapply(functions, source)  # Read all data frames

# Note: You will need to install all packages prior to loading them.
packages <- c("dplyr",
              "ggplot2",
              "ggpubr",
              "ggthemes",
              "gridExtra",
              "hrbrthemes",
              "patchwork",
              "Matrix",
              "pracma",
              "scales",
              "RColorBrewer")

# Load
lapply(packages, require, character.only = TRUE)

#
# Make sure you have the tools package
library(tools)

# Function to read the CSV, fit the model, and create a data frame with additional columns
process_file <- function(file_path, race, er_status) {
  # Read file into rates (adjust csv2rates according to your function's needs)
  rates <- csv2rates(file_path)
  
  # Fit the APC model
  fit <- apc2fit(rates)
  
  fitted_temporal_trends <- as.data.frame(fit$FittedTemporalTrends)
     
  #   # Add race and er_status columns
     fitted_temporal_trends$race <- race
     fitted_temporal_trends$er <- er_status
  #   
     return(fitted_temporal_trends)
}

# Base directory path where files are stored
base_path <- "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype"

# Vector of file identifiers
file_identifiers <- c("nhw_HR+_HER2+", "nhw_HR+_HER2-", "nhw_HR-_HER2+", "nhw_HR-_HER2-", 
                      "nhb_HR+_HER2+", "nhb_HR+_HER2-", "nhb_HR-_HER2+", "nhb_HR-_HER2-",
                      "his_HR+_HER2+", "his_HR+_HER2-", "his_HR-_HER2+", "his_HR-_HER2-",
                      "api_HR+_HER2+", "api_HR+_HER2-", "api_HR-_HER2+", "api_HR-_HER2-")

# Initialize an empty list to store results
all_results <- list()

# Loop through each file identifier
for (identifier in file_identifiers) {
  # Construct the full file path
  file_path <- file.path(base_path, paste0(identifier, ".csv"))
  
  # Split identifier into race and ER status parts
  parts <- unlist(strsplit(identifier, "_"))
  race <- toupper(parts[1])
  er_status <- toTitleCase(parts[2])
  
  # Process the file and store the result in the list
  all_results[[identifier]] <- process_file(file_path, race, er_status)
}

# If you want a combined data frame with all results
combined_results <- do.call(rbind, all_results)

# View the combined results
print(combined_results)

FTT<-combined_results

#write.csv(FTT, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/FTT.csv", row.names = FALSE)


#
# Make sure you have the tools package
library(tools)

# Function to read the CSV, fit the model, and create a data frame with additional columns
process_file <- function(file_path, race, er_status) {
  # Read file into rates (adjust csv2rates according to your function's needs)
  rates <- csv2rates(file_path)
  
  # Fit the APC model
  fit <- apc2fit(rates)
  
  NetDrift <- as.data.frame(fit$NetDrift)
  
  #   # Add race and er_status columns
  NetDrift$race <- race
  NetDrift$er <- er_status
  #   
  return(NetDrift)
}

# Base directory path where files are stored
base_path <- "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype"

# Vector of file identifiers
file_identifiers <- c("nhw_HR+_HER2+", "nhw_HR+_HER2-", "nhw_HR-_HER2+", "nhw_HR-_HER2-", 
                      "nhb_HR+_HER2+", "nhb_HR+_HER2-", "nhb_HR-_HER2+", "nhb_HR-_HER2-",
                      "his_HR+_HER2+", "his_HR+_HER2-", "his_HR-_HER2+", "his_HR-_HER2-",
                      "api_HR+_HER2+", "api_HR+_HER2-", "api_HR-_HER2+", "api_HR-_HER2-")

# Initialize an empty list to store results
all_results <- list()

# Loop through each file identifier
for (identifier in file_identifiers) {
  # Construct the full file path
  file_path <- file.path(base_path, paste0(identifier, ".csv"))
  
  # Split identifier into race and ER status parts
  parts <- unlist(strsplit(identifier, "_"))
  race <- toupper(parts[1])
  er_status <- toTitleCase(parts[2])
  
  # Process the file and store the result in the list
  all_results[[identifier]] <- process_file(file_path, race, er_status)
}

# If you want a combined data frame with all results
combined_results <- do.call(rbind, all_results)

# View the combined results
print(combined_results)

NetDrift<-combined_results

#write.csv(NetDrift, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/subtype/NetDrift.csv", row.names = FALSE)

