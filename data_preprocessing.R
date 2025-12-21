# Load the original data
data_path = paste0(getwd(), "/asian/ASIAN.DAT")

data_raw <- toString(readLines(data_path))

data_raw <- strsplit(data_raw, ", ")
data <- data.frame(data_raw)
colnames(data) <- "raw"

# Extract columns

extract_columns <- function(idxs, df){
  column = sapply(data, substr, start = idxs[1], stop = idxs[2])
  return(column)
}



col_idxs = list(c(1, 4),
                c(5, 7),
                c(8, 12),
                c(13, 16))

col_list <- lapply(col_idxs, extract_columns, df = data)

data$ChildID <- as.vector(col_list[[1]])
data$Age <- as.vector(col_list[[2]])
data$Weight <- as.vector(col_list[[3]])
data$Birthweight <- as.vector(col_list[[4]])

data <- data[,-1]

# Preprocess columns
data[,2:4] <- sapply(data[,2:4], as.integer)

data$ChildID <- gsub(" ", "", data$ChildID)
data$ChildID <- as.factor(data$ChildID)

# Use birthweight as Weight at Age 0
birthweight <- data[,c("ChildID", "Birthweight")]
birthweight <- birthweight[(!duplicated(birthweight)),]
birthweight$Age <- 0
birthweight$Weight <- birthweight$Birthweight


data_wide <- data
data_long <- rbind(data[,c("ChildID", "Age", "Weight")],
                   birthweight[,c("ChildID", "Age", "Weight")])


# Normalize / logarithm of the columns
data_long$log_Age <- log(data_long$Age + 1)
data_long$log_Weight <- log(data_long$Weight)

data_wide$n_Age <- (data_wide$Age - mean(data_wide$Age)) / sd(data_wide$Age)
data_wide$n_Weight <- (data_wide$Weight - mean(data_wide$Weight)) / sd(data_wide$Weight)
data_wide$log_Age <- log(data_wide$Age + 1)
data_wide$log_Weight <- log(data_wide$Weight)

# Number of observations
data_long$NObs <- as.vector(table(data_long$ChildID)[data_long$Child])
data_wide$NObs <- as.vector(table(data_wide$ChildID)[data_wide$Child])

# remove unneccessary objects from the environment
rm(data_raw, data, col_list, col_idxs, birthweight, data_path, extract_columns)



head(data_long)
head(data_wide)

dim(data_long)
dim(data_wide)


