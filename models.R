library("lme4")
library("lmerTest")
library("ggplot2")
library("dplyr")
library("lmerTest")

####################################
###             Plan             ###
####################################

## Preparation
# Load the script for data preprocessing
source("data_preprocessing.R")

# We will use rescaled values for both time and target values:
# - centered Age in years
# - centered Weight / Birthweight in kg 

df_wide = data_wide[c("ChildID", "Gender", "NObs", "GenderID",
                      "c_Weight_kg", "c_Birthweight_kg", "c_Age_years")]

colnames(df_wide) <- c("ChildID", "Gender", "NObs", "GenderID",
                       "Weight", "Birthweight", "Age")

# Here birth weight is considered to be observation at time point 0.
# This data will be used later for comparison.

df_long = data_long[c("ChildID", "Gender", "NObs", "GenderID",
                      "r_Weight", "r_Age")]

colnames(df_long) <- c("ChildID", "Gender", "NObs", "GenderID",
                        "Weight", "Age")

rm(verbose)

# We will fit different models of increasing complexity on the same data
# and conduct model comparison using criteria.

####################################
###        Model fitting         ###
####################################

## wide data

#------- only age as predictor --------------------
model01 <- lmer(Weight ~ 1 + (1|ChildID), 
                    data=df_wide, REML=FALSE)
model02 <- lmer(Weight ~ 1 + Age + (1|ChildID), 
                    data=df_wide, REML=FALSE)
model03 <- lmer(Weight ~ 1 + Age + (1 + Age|ChildID), 
                    data=df_wide, REML=FALSE)

#------- time-invariant predictors ----------------
model04 <- lmer(Weight ~ 1 + Age + Birthweight + (1 + Age|ChildID), 
                data=df_wide, REML=FALSE)
model05 <- lmer(Weight ~ 1 + Age * Birthweight + (1 + Age|ChildID), 
                data=df_wide, REML=FALSE)
model06 <- lmer(Weight ~ 1 + Age + GenderID + (1 + Age|ChildID), 
                data=df_wide, REML=FALSE)
model07 <- lmer(Weight ~ 1 + Age * GenderID + (1 + Age|ChildID), 
                data=df_wide, REML=FALSE)
model08 <- lmer(Weight ~ 1 + Age * Birthweight + Age * GenderID + (1 + Age|ChildID), 
                data=df_wide, REML=FALSE)

#------- non-linear growth ------------------------
model09 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1|ChildID), 
                data=df_wide, REML=FALSE)
model10 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1 + Age + I(Age^2)|ChildID), 
                    data=df_wide, REML=FALSE)
model11 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1 + Age + I(Age^2)|ChildID) +
                  Age * Birthweight + Age * GenderID, 
                data=df_wide, REML=FALSE)

## long data

#model12 <- lmer(Weight ~ 1 + (1|ChildID), 
#                data=df_long, REML=FALSE)

#model13 <- lmer(Weight ~ 1 + (1|ChildID), 
#                data=df_long, REML=FALSE)

#model14 <- lmer(Weight ~ 1 + (1|ChildID), 
#                data=df_long, REML=FALSE)

#############################
#####    Save results   #####
#############################

df_result = df_wide[,c("ChildID", "Age", "Weight", "Birthweight", "Gender")]

df_result["m01_fitted"] <- fitted(model01)
df_result["m02_fitted"] <- fitted(model02)
df_result["m03_fitted"] <- fitted(model03)
df_result["m04_fitted"] <- fitted(model04)
df_result["m05_fitted"] <- fitted(model05)
df_result["m06_fitted"] <- fitted(model06)
df_result["m07_fitted"] <- fitted(model07)
df_result["m08_fitted"] <- fitted(model08)
df_result["m09_fitted"] <- fitted(model09)
df_result["m10_fitted"] <- fitted(model10)
df_result["m11_fitted"] <- fitted(model11)

#################################
##### Result visualization ######
#################################

plot_results <- function(data, model_id, model_name,
                         n = 10, ncols = 5){
  
  first_n_id <- as.vector(unique(data$ChildID[order(data$ChildID)]))[1:n]
  first_n_id_idx <- which(data$ChildID %in% first_n_id)
  
  data_n <- data[first_n_id_idx,]
  pred_name = paste0("m", model_id, "_fitted")
  
  g = ggplot(data_n, aes(x = Age, y = Weight)) +
    geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
    geom_line(aes(group=ChildID)) + 
    facet_wrap(~ChildID, ncol=ncols)+
    scale_x_continuous(name = "Age") + 
    scale_y_continuous(name = "Weight") +
    geom_line(aes(y=.data[[pred_name]], col=ChildID), lwd=1.5)
  
  return(g)
  #return(list("plot" = g, "fitted_values" = pred_name))
}

plot_results(data = df_result, model_id = "01", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "02", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "03", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "04", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "05", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "06", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "07", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "08", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "09", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "10", 
             model_name = "Model 1: within-person empty model")
plot_results(data = df_result, model_id = "11", 
             model_name = "Model 1: within-person empty model")
