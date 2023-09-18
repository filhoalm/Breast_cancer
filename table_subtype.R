####
# Table
###


data <- read.csv("https://raw.githubusercontent.com/filhoalm/Breast_cancer/main/data/data_morphology/data_morpho_9162023.csv")
data <- data[c(-1)]

indice <- unique(data$index) # we get the unique index values

# we'll write a function that does all the steps for one of the data$index unique values
clean_data <- function(i, data) {
  
  # we standardise i to match the format in the data dataframe
  id <- gsub("-", "_", i)
  id <- gsub(" ", "", id)
  
  a <- subset(data, data$site == "DCIS" & data$index == id & data$year >= 1975)
  b <- subset(data, data$site == "IDC" & data$index == id & data$year >= 1975)
  c <- cbind(a,b)[c(6,2,3,7,11,12,16)]
  
  colnames(c) <- c("year", "asr_dcis", "cases_dcis","rr_dcis", "asr_idc", "cases_idc","rr_idc")
  c$total_cases <- c$cases_dcis + c$cases_idc
  c$prop_dcis <- round((c$cases_dcis / c$total_cases)*100, 2)
  c$prop_idc <- round((c$cases_idc / c$total_cases)*100, 2)
  e <- c[c(1,2,3,5,6,8,9,10,4,7)]
  e$index <- paste0(i)
  
  return(e)
}

# we apply the function to each index value with lapply and we rbind the lists into one dataframe
df <- do.call("rbind", lapply(indice, clean_data, data))

#write.csv(df, 'C:/Users/filhoam/Desktop/Breast/Round2/table_subtype_9162023.csv')

