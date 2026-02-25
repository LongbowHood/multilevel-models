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
                      "r_Weight", "r_Age_weeks")]

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
                    data=df_wide, REML=TRUE)
model02 <- lmer(Weight ~ 1 + Age + (1|ChildID), 
                    data=df_wide, REML=TRUE)
model03 <- lmer(Weight ~ 1 + Age + (1 + Age|ChildID), 
                    data=df_wide, REML=TRUE)

#------- time-invariant predictors ----------------
model04 <- lmer(Weight ~ 1 + Age + Birthweight + (1 + Age|ChildID), 
                data=df_wide, REML=TRUE)
model05 <- lmer(Weight ~ 1 + Age * Birthweight + (1 + Age|ChildID), 
                data=df_wide, REML=TRUE)
model06 <- lmer(Weight ~ 1 + Age + GenderID + (1 + Age|ChildID), 
                data=df_wide, REML=TRUE)
model07 <- lmer(Weight ~ 1 + Age * GenderID + (1 + Age|ChildID), 
                data=df_wide, REML=TRUE)
model08 <- lmer(Weight ~ 1 + Age * Birthweight + Age * GenderID + (1 + Age|ChildID), 
                data=df_wide, REML=TRUE)

#------- non-linear growth ------------------------
model09 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1|ChildID), 
                data=df_wide, REML=TRUE)
model10 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1 + Age + I(Age^2)|ChildID), 
                    data=df_wide, REML=TRUE)
model11 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1 + Age + I(Age^2)|ChildID) +
                  Age * Birthweight + Age * GenderID, 
                data=df_wide, REML=TRUE)


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

time_grid <- seq(min(df_wide["Age"]), max(df_wide["Age"]), length.out = 50)

plot_results <- function(data, model, model_name,
                         n = 10, ncols = 5){
  set.seed(42)
  sample_n_id <- sample(as.vector(unique(data$ChildID[order(data$ChildID)])), n)
  sample_n_id_idx <- which(data$ChildID %in% first_n_id)
  data_n <- unique(data[first_n_id_idx, 
                        c("ChildID", "Birthweight", "GenderID", "Age", "Weight")])
  
  newdata <- cbind(data_n, rep(rownames(data_n), each = length(time_grid)))[,1:3]
  newdata$Age <- rep(time_grid, each = n)
  
  newdata$Predictions <- predict(model, newdata)
  
  g = ggplot(data_n, aes(x = Age, y = Weight)) +
    geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
    geom_line(aes(group=ChildID)) + 
    facet_wrap(~ChildID, ncol=ncols)+
    scale_x_continuous(name = "Age") + 
    scale_y_continuous(name = "Weight") +
    geom_line(data = newdata, 
              aes(x = Age, y = Predictions, group = ChildID, color = factor(ChildID)),
              lwd=1.5) +
    ggtitle(model_name)
  
  return(g)
  #return(list("plot" = g, "fitted_values" = pred_name))
}


plot_results(df_wide, model01, 
             model_name = "Model 1: within-person empty model")
plot_results(df_wide, model02, 
             model_name = "Model 2: random intercept, fixed linear time")
plot_results(df_wide, model03, 
             model_name = "Model 3: random linear time (RLT)")
plot_results(df_wide, model04, 
             model_name = "Model 4: RLT + fixed Birthweight")
plot_results(df_wide, model05, 
             model_name = "Model 5: RLT + fixed Birthweight with interaction")
plot_results(df_wide, model06, 
             model_name = "Model 6: RLT + fixed Gender")
plot_results(df_wide, model07, 
             model_name = "Model 7: RLT + fixed Birthweight and Gender")
plot_results(df_wide, model08, 
             model_name = "Model 8: RLT + fixed Birthweight and Gender with interactions")
plot_results(df_wide, model09, 
             model_name = "Model 9: random intercept, fixed quadratic time")
plot_results(df_wide, model10, 
             model_name = "Model 10: random quadratic time")
plot_results(df_wide, model11, 
             model_name = "Model 11: RQT + fixed Birthweight and Gender with interactions")


plot(model11)
