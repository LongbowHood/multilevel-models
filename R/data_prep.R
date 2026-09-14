#' Get processed multilevel data
#' 
#' @param format Either "wide" or "long" (though models primarily use wide)
#' @return A data frame of the cleaned and scaled dataset
get_multilevel_data <- function(format = "wide") {
  processed_path <- paste0("data/processed_data/cleaned_data_", format, ".csv")
  
  if (file.exists(processed_path)) {
    message("Loading cached Multilevel Growth data...")
    return(read.csv(processed_path, stringsAsFactors = TRUE))
  }
  
  message("Processing raw Multilevel Growth data from scratch...")
  
  # Load the original data
  data_path <- "data/raw_data/ASIAN.DAT"
  data_raw <- toString(readLines(data_path))
  data_raw <- strsplit(data_raw, ", ")[[1]]
  
  # Extract fixed-width columns
  extract_columns <- function(idxs, vec){
    sapply(vec, substr, start = idxs[1], stop = idxs[2])
  }
  
  col_idxs <- list(c(1, 4), c(5, 7), c(8, 12), c(13, 16), c(17, 17))
  col_list <- lapply(col_idxs, extract_columns, vec = data_raw)
  
  data <- data.frame(
    ChildID = as.factor(gsub(" ", "", col_list[[1]])),
    Age = as.integer(col_list[[2]]),
    Weight = as.integer(col_list[[3]]),
    Birthweight = as.integer(col_list[[4]]),
    Gender = col_list[[5]]
  )
  
  data$NObs <- as.vector(table(data$ChildID)[data$ChildID])
  data$GenderID <- as.integer(data$Gender) - 1
  data$Gender <- factor(data$GenderID, labels = c("Boy", "Girl"))
  
  if (format == "wide") {
    # Exclude the data with 1 observation only (not longitudinal data)
    data <- data[data$NObs > 1, ]
    
    # Rescale variables
    data$r_Weight <- data$Weight / 1000
    data$r_Birthweight <- data$Birthweight / 1000
    data$r_Age_weeks <- data$Age / 7 
    data$r_Age_years <- data$Age / 365
    
    # Centralization
    data$c_Age <- data$Age - mean(data$Age)
    data$c_Weight <- data$Weight - mean(data$Weight)
    data$c_Birthweight <- data$Birthweight - mean(data$Birthweight)
    
    data$c_Age_years <- data$r_Age_years - mean(data$r_Age_years)
    data$c_Weight_kg <- data$r_Weight - mean(data$r_Weight)
    data$c_Birthweight_kg <- data$r_Birthweight - mean(data$r_Birthweight)
  }
  
  write.csv(data, processed_path, row.names = FALSE)
  return(data)
}
