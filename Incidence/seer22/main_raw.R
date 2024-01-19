

# Load the data from the GitHub repository as before
file_url <- "https://raw.githubusercontent.com/filhoalm/Breast_cancer/main/Incidence/seer22_er_race.csv"
breast_cancer_data <- read.csv(url(file_url), stringsAsFactors = FALSE)
names(breast_cancer_data)
# Rename the variables to the desired names
colnames(breast_cancer_data) <- c("age", "year", "race", "er", "rate", "cases", "py")

# View the first few rows with the new variable names
head(breast_cancer_data)
unique(breast_cancer_data$year)
breast_cancer_data$year <- as.numeric(breast_cancer_data$year) + 1999
# Assuming your data frame is named 'breast_cancer_data' and it has columns 'age', 'race', 'subtype', 'cases', and 'py'
# First, categorize age into the age groups if it hasn't been done already
age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)
age_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

# 1x1 resolution data
breast_cancer_data$age_group <- cut(breast_cancer_data$age, breaks=age_breaks, labels=age_labels, right=FALSE, include.lowest=TRUE)
head(breast_cancer_data)
# 5x1 resolution data
# Now, aggregate both cases and py by the new age groups as well as by race and subtype
data <- aggregate(cbind(cases, py) ~ age_group + race + year + er, data=breast_cancer_data, sum)
data$year <- as.numeric(data$year) + 1999
# Check the result
# Ensure there's an id variable for reshaping
breast_cancer_data$id <- with(breast_cancer_data, paste0(age, "_", race, "_", er))

# Reshape 'cases' to wide format
bc_cases_wide <- reshape(breast_cancer_data, idvar = "id", timevar = "year", 
                         direction = "wide", v.names = "cases")

# Reshape 'py' to wide format
bc_py_wide <- reshape(breast_cancer_data, idvar = "id", timevar = "year", 
                      direction = "wide", v.names = "py")

# Merge the 'cases' and 'py' data frames
merged_data <- merge(bc_cases_wide, bc_py_wide, by = "id", all = TRUE)

# Create a vector with the sorted year column names
years <- unique(breast_cancer_data$year)
sorted_colnames <- unlist(lapply(years, function(y) c(paste("cases", y, sep = "."), paste("py", y, sep = "."))))

# Select columns in the order of 'id', then alternating 'cases.YEAR' and 'py.YEAR'
final_data <- merged_data[c("id", sorted_colnames)]

# View the results
head(final_data)

# Split the 'id' back into 'age', 'race', and 'er'
split_id <- strsplit(as.character(final_data$id), "_")
final_data$age <- as.numeric(sapply(split_id, `[`, 1))
final_data$race <- as.numeric(sapply(split_id, `[`, 2))
final_data$er <- as.numeric(sapply(split_id, `[`, 3))

# Recode 'race' into a factor with appropriate labels
final_data$race <- factor(final_data$race, levels = c(0, 1, 2, 3, 4, 5),
                                  labels = c("NHW", "NHB", "AIAN", "API", "HIS", "Unknown"))

# Recode 'er' into a factor with appropriate labels
final_data$er <- factor(final_data$er, levels = c(0, 1, 2, 3),
                                labels = c("Positive", "Negative", "Borderline/Unknown", "Recode not available"))

# Exclude the 'id' variable
final_data$id <- NULL

# Reorder columns so 'age', 'race', and 'er' are the first columns
final_data <- final_data %>%
  select(age, race, er, everything())

###############################

head(final_data)

# output_dir <- "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/"
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }



{

# NHW with Positive ER status
df <- subset(final_data, age >= 35 & age <= 84)
df1 <- subset(df, race == "NHW" & er == "Positive")
df2 <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed

# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: NHW - Positive"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
combined_data_nhw_positive <- rbind(summary_info, df2)

# View the combined data
head(combined_data_nhw_positive)

# Save the combined data to a CSV file, modifying the filename as needed
write.csv(combined_data_nhw_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/nhw_positive.csv", row.names = FALSE)



# NHW with Negative ER status
df <- subset(final_data, age >= 35 & age <= 84)
df1 <- subset(df, race == "NHW" & er == "Negative")
df2 <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed

# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: NHW - Negative"
summary_info[3, 1] <- "Start Year: 2010"  # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"     # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"  # Enter the actual interval if available

# Combine the summary information with the data
combined_data_nhw_negative <- rbind(summary_info, df2)

# View the combined data
head(combined_data_nhw_negative)

# Save the combined data to a CSV file, modifying the filename as needed
write.csv(combined_data_nhw_negative, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/nhw_negative.csv", row.names = FALSE)



# NHB with Positive ER status
df <- subset(final_data, age >= 35 & age <= 84)
df1 <- subset(df, race == "NHB" & er == "Positive")
df2 <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed

# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: NHB - Positive"
summary_info[3, 1] <- "Start Year: 2010"  # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"     # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"  # Enter the actual interval if available

# Combine the summary information with the data
combined_data_nhb_positive <- rbind(summary_info, df2)

# View the combined data
head(combined_data_nhb_positive)

# Save the combined data to a CSV file, modifying the filename as needed
write.csv(combined_data_nhb_positive, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/nhb_positive.csv", row.names = FALSE)


# NHB with Negative ER status
df <- subset(final_data, age >= 35 & age <= 84)
df1 <- subset(df, race == "NHB" & er == "Negative")
df2 <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed

# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: NHB - Negative"
summary_info[3, 1] <- "Start Year: 2010"  # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"     # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"  # Enter the actual interval if available

# Combine the summary information with the data
combined_data_nhb_negative <- rbind(summary_info, df2)

# View the combined data
head(combined_data_nhb_negative)

# Save the combined data to a CSV file, modifying the filename as needed
write.csv(combined_data_nhb_negative, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/nhb_negative.csv", row.names = FALSE)

###########
# HIS with Negative ER status
df <- subset(final_data, age >= 35 & age <= 84)
df1 <- subset(df, race == "HIS" & er == "Negative")
df2 <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed

# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: HIS - Negative"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
df6 <- rbind(summary_info, df2)

# View the combined data
head(df6)

# Save the combined data to a CSV file, modifying the filename as needed
write.csv(df6, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/his_negative.csv", row.names = FALSE)


###
# HIS with Positive ER status
df <- subset(final_data, age >= 35 & age <= 84)
df1 <- subset(df, race == "HIS" & er == "Positive")
df2 <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed

# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: HIS - Positive"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
df6 <- rbind(summary_info, df2)

# View the combined data
head(df6)

# Save the combined data to a CSV file, modifying the filename as needed
write.csv(df6, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/his_positive.csv", row.names = FALSE)


##############
# API with Negative ER status
df <- subset(final_data, age >= 35 & age <= 84)
df1 <- subset(df, race == "API" & er == "Negative")
df2 <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed

# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: API - Negative"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
df6 <- rbind(summary_info, df2)

# View the combined data
head(df6)

# Save the combined data to a CSV file, modifying the filename as needed
write.csv(df6, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/api_negative.csv", row.names = FALSE)

##############
# API with Positive ER status
df <- subset(final_data, age >= 35 & age <= 84)
df1 <- subset(df, race == "API" & er == "Positive")
df2 <- df1[-(1:25)]  # Assuming you are removing columns for years not between 2010-2020, adjust as needed

# Create a dataframe with a single row and the same number of columns as df2
summary_info <- data.frame(matrix(NA, ncol = length(df2), nrow = 5))
colnames(summary_info) <- colnames(df2)

# Fill in the summary information
summary_info[1, 1] <- "Title: Breast Cancer"
summary_info[2, 1] <- "Description: API - Positive"
summary_info[3, 1] <- "Start Year: 2010"               # Enter the actual Start Year if available
summary_info[4, 1] <- "Start Age: 35"                  # Enter the actual Start Age if available
summary_info[5, 1] <- "Interval (Years): 1"            # Enter the actual interval if available

# Combine the summary information with the data
df6 <- rbind(summary_info, df2)

# View the combined data
head(df6)

# Save the combined data to a CSV file, modifying the filename as needed
write.csv(df6, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/api_positive.csv", row.names = FALSE)


} # generate rate objects



########

# Explore webtool functions


################################
# Summer Project - Kidney cancer
#
################################

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
  
  # Extract the fitted temporal trends and convert to data frame
  NetDrift <- as.data.frame(fit$NetDrift)
  
  # Add race and er_status columns
  NetDrift$race <- race
  NetDrift$er <- er_status
  
  return(NetDrift)
}

# Base directory path where files are stored
base_path <- "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/exclude2020/"

# Vector of file identifiers
file_identifiers <- c("nhw_positive", "nhw_negative", "nhb_positive", "nhb_negative", 
                      "his_positive", "his_negative", "api_positive", "api_negative")

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



# Define a vector of column names to round
columns_to_round <- c("Rate", "CI Lo", "CI Hi")

# Loop through each column name and apply the rounding function
for (col in columns_to_round) {
  combined_results[[col]] <- round(combined_results[[col]], 0)
}

#write.csv(combined_results, "C:/Users/filhoam/Desktop/Breast/Incidence/seer22/nedrift_race_er.csv", row.names = FALSE)


####




