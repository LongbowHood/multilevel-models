rm(list = ls())

# Load the original data
data_path = paste0(getwd(), "/asian/ASIAN.DAT")

# Read the file line by line and convert to one single string
data_raw <- toString(readLines(data_path))

# Split the long string wherever there is ", " (comma + space)
data_raw <- strsplit(data_raw, ", ") # 1572

# Creates a one-column data frame
data <- data.frame(data_raw) # 1572 x 1
colnames(data) <- "raw"


# Extract columns
# df is a one-column data frame where each row contains one full string
extract_columns <- function(idxs, df){
  column = sapply(df, substr, start = idxs[1], stop = idxs[2])
  return(column)
}

# Define start and end positions for each column (fixed-width format)
col_idxs = list(c(1, 4),    # ChildID: characters 1–4
                c(5, 7),    # Age: characters 5–7
                c(8, 12),   # Weight: characters 8–12
                c(13, 16),  # Birthweight: characters 13–16
                c(17, 17))  # Gender: character 17

col_list <- lapply(col_idxs, extract_columns, df = data) # 5

# Assign extracted columns to new variables in the data frame
data$ChildID <- as.vector(col_list[[1]])
data$Age <- as.vector(col_list[[2]])
data$Weight <- as.vector(col_list[[3]])
data$Birthweight <- as.vector(col_list[[4]])
data$Gender <- as.vector(col_list[[5]])

data <- data[,-1]
data

# Preprocess columns
data[, 2:4] <- sapply(data[,2:4], as.integer)

data$ChildID <- gsub(" ", "", data$ChildID)

as.integer(data$ChildID)

data$ChildID <- as.factor(data$ChildID)


## This is long repeated measurement data
head(data) # 1572 x 5

## birthweight dataset alone
# Use birthweight as Weight at Age 0
birthweight <- data[, c("ChildID", "Birthweight", "Gender")] # 1572 x 3

# remove the duplicates of birthweight and keep the birthweght of 568 children
birthweight <- birthweight[(!duplicated(birthweight)),]
birthweight$Age <- 0 # add column of Age = 0 as birthweight
birthweight$Weight <- birthweight$Birthweight # add another column of weight

head(birthweight)

# repeated measurements are stored in different columns.
data_wide <- data 

data_long <- rbind(data[,c("ChildID", "Age", "Weight", "Gender")],
                   birthweight[,c("ChildID", "Age", "Weight", "Gender")]) 

# (1572 + 568) x 4 
head(data_long) # 2140 x 4


# Number of observations
data_long$NObs <- as.vector(table(data_long$ChildID)[data_long$Child])
data_wide$NObs <- as.vector(table(data_wide$ChildID)[data_wide$Child]) # ??

# Decode gender
data_wide$GenderID <- as.integer(data_wide$Gender) - 1
data_wide$Gender <- factor(data_wide$GenderID, labels = c("Boy", "Girl"))

data_long$GenderID <- as.integer(data_long$Gender) - 1
data_long$Gender <- factor(data_long$GenderID, labels = c("Boy", "Girl"))

head(data_long) # 2140 x 6
head(data_wide)
 
# remove unneccessary objects from the environment
rm(data_raw, data, col_list, col_idxs, birthweight, data_path, extract_columns)

