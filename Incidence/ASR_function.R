# Load the data from the GitHub repository as before
file_url <- "https://raw.githubusercontent.com/filhoalm/Breast_cancer/main/Incidence/breast_seer12_subtype_race.csv"
breast_cancer_data <- read.csv(url(file_url), stringsAsFactors = FALSE)

# Rename the variables to the desired names
colnames(breast_cancer_data) <- c("year", "age", "race", "subtype", "rate", "cases", "py", "er")

# View the first few rows with the new variable names
head(breast_cancer_data)

# Assuming your data frame is named 'breast_cancer_data' and it has columns 'age', 'race', 'subtype', 'cases', and 'py'

# First, categorize age into the age groups if it hasn't been done already
age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)
age_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

breast_cancer_data$age_group <- cut(breast_cancer_data$age, breaks=age_breaks, labels=age_labels, right=FALSE, include.lowest=TRUE)

# Now, aggregate both cases and py by the new age groups as well as by race and subtype
data <- aggregate(cbind(cases, py) ~ age_group + race + year + subtype, data=breast_cancer_data, sum)
data$year <- as.numeric(data$year) + 1991
# Check the result
head(data)
data<-subset(data, data$year > 1991)
unique(data$year)

# Recode the race and stage values
data$race <- factor(data$race, levels = c(0, 1, 2, 3, 4, 5),
                    labels = c("NHW", "NHB", "AIAN", "API", "HIS", "Unknown"))
data$subtype <- factor(data$subtype, levels = c(0, 1, 2, 3, 4, 5),
                       labels = c("HR+/HER2+", "HR-/HER2+", "HR+/HER2-", "HR-/HER2-", "Unknown", "Recode not available"))

# U.S 2000 Standard population
us_2000_std_pop <- c(
  '0-4'   = 19167602,
  '5-9'   = 20099441,
  '10-14' = 20677153,
  '15-19' = 21376861,
  '20-24' = 21453335,
  '25-29' = 21154684,
  '30-34' = 19943469,
  '35-39' = 20873546,
  '40-44' = 22723816,
  '45-49' = 22263074,
  '50-54' = 19613129,
  '55-59' = 16574307,
  '60-64' = 13407195,
  '65-69' = 11049005,
  '70-74' = 8602216,
  '75-79' = 6625462,
  '80-84' = 4919015,
  '85+'   = 5127427
)

calculate_asir <- function(data, standard_population) {
  # Ensure that age_group matches standard_population names
  if (!all(data$age_group %in% names(standard_population))) {
    stop("Age groups in data do not match standard population age groups.")
  }
  
  # Calculate age-specific incidence rates for each row in the data
  data$age_specific_rate <- data$cases / data$py
  
  # Standardize incidence rates using the standard population
  data$standardized_rate <- mapply(function(age_group, rate) {
    return (rate * standard_population[age_group])
  }, age_group = data$age_group, rate = data$age_specific_rate)
  
  # Aggregate the standardized rates by race and subtype
  asir <- aggregate(standardized_rate ~ race + year + subtype, data, sum)
  
  # Normalize by the total standard population to get ASIR
  total_standard_population <- sum(standard_population)
  asir$asir <- asir$standardized_rate / total_standard_population * 1e5 # per 100,000 population
  
  return(asir[, c('year', 'race', 'subtype', 'ASIR')])
}

# Example usage
# Your data should include the 'age_group', 'race', 'subtype', 'cases', and 'py' columns matching standard_population names.
asr <- as.data.frame(calculate_asir(data, us_2000_std_pop))
asr$asir<-round((asr$asir),2)
head(asr)