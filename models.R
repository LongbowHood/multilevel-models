#load the script for data preprocessing
#...

# Imports
library("lme4")
library("lmerTest")
library("ggplot2")
library("dplyr")
library("lmerTest")

#load the data
data_path = "/prep_data"

df_5_long = read.csv2(file = data_path + "/data__5_obs_long.csv", header = TRUE, sep = ",", dec = ".")
