#load the script for data preprocessing
source("data_preprocessing.R")

# Imports
library("lme4")
library("lmerTest")
library("ggplot2")
library("dplyr")
library("lmerTest")

#load the data
data_path = "/prep_data"

#divide data into slices
df5_long <- data_long[data_long$NObs == 5,]

# data exploration

pairs(df5_long)
