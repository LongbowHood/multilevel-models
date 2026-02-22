# Load the script for data preprocessing
source("data_preprocessing.R")

# Imports
library("lme4")
library("lmerTest")
library("ggplot2")
library("dplyr")
library("lmerTest")

####################################
###             Plan             ###
####################################
# We will model the same data in several different ways:
# 1) using Age as the only predictor on observations after birth
# 2) using Age as time-variant and Birthweight as time-invariant predictors
# 3) using Age as time-variant and Birthweigt and Gender as time-invariant predictors
# 4) using Age as the only predictor (Birthweight as observation at 0)
# 5) using Age as time-variant and Gender as time-invariant predictor (Birthweight as observation at 0)

# We will use rescaled values for both time and target values:
# - Age in weeks
# - Weight / Birthweight in kg 

## For cases (1), (2), (3) we use data_wide.
## For cases (4) and (5) we use data_long.

df_wide = data_wide[c("ChildID", "Gender", "NObs", "GenderID",
                      "r_Weight", "r_Birthweight", "r_Age")]

df_long = data_long[c("ChildID", "Gender", "NObs", "GenderID",
                      "r_Weight", "r_Age")]

## Preparation

colnames(df_wide) <- c("ChildID", "Gender", "NObs", "GenderID",
                       "Weight", "Birthweight", "Age")
colnames(df_long) <- c("ChildID", "Gender", "NObs", "GenderID",
                        "Weight", "Age")
df_wide = df_wide[df_wide["NObs"] > 2, ]

rm(verbose)

####################################
###        Model fitting         ###
####################################

######## Case 1 ########
# wide data
# Age as only predictor
########################
df_result1 = df_wide[,c("ChildID", "Age", "Weight", "Gender")]

model1_rint <- lmer(Weight ~ 1 + (1|ChildID), 
                    data=df_wide, REML=FALSE)
# didn't fit -> boundary (singular) fit: see help('isSingular')
df_result1["pred_model1_rint"] <- fitted(model1_rint)

model1_rint_fslp <- lmer(Weight ~ 1 + Age + (1|ChildID),
                          data=df_wide, REML=FALSE)

model1_rint_rslp <- lmer(Weight ~ 1 + Age + (1 + Age|ChildID),
                          data=df_wide, REML=FALSE)
### -> didn't fit

model1_rint_rslp_quad <- lmer(Weight ~ 1 + Age + I(Age^2) + (1 + Age + I(Age^2)|ChildID),
                         data=df_wide, REML=FALSE)

# long data

#model_l_rint <- lmer(log_Weight ~ 1 + (1|ChildID), 
#                     data=df5_long, REML=FALSE)
### -> didn't fit

#model_l_rint_fslp <- lmer(n_Weight ~ 1 + log_Age + I(log_Age^2) + 
#                            (1|ChildID),
#                          data=df5_long, REML=FALSE)

#model_l_rint_rslp <- lmer(log_Weight ~ 1 + log_Age + (1 + log_Age|ChildID) ,
#                          data=df5_long, REML=FALSE)
### -> didn't fit


##Result visualization
plot_results <- function(data, model, model_name,
                         n = 10, ncols = 5, slopes = FALSE quad=FALSE){
  
  
  first_n_id <- as.vector(unique(data$ChildID[order(data$ChildID)]))[1:n]
  first_n_id_idx <- which(data$ChildID %in% first_n_id)
  
  data <- fitted(model)
  data_n <- data[first_n_id_idx,]
  
  g = ggplot(data_n, aes(x = Age, y = Weight)) +
    geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
    geom_line(aes(group=ChildID)) + 
    facet_wrap(~ChildID, ncol=ncols)+
    scale_x_continuous(name = "Age") + 
    scale_y_continuous(name = "Weight") +
    geom_line(aes(y=quad_pred, col=subID), lwd=1.5)
  13
  
  
  if(slopes){
    pred_model <- data.frame(ChildID=first_n_id, 
                             Intercepts=c(coef(model)$ChildID[first_n_id,1]), 
                             Slopes=c(coef(model)$ChildID[first_n_id,2]))
  }else{
    pred_model <- data.frame(ChildID=first_n_id, 
                             Intercepts=c(coef(model)$ChildID[first_n_id,1]), 
                             Slopes=rep(0, n))
  }
  
  g <- ggplot(first_n, aes(x = Age, y = Weight)) + 
              geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
              geom_line(aes(group=ChildID)) + 
              facet_wrap(~ChildID, ncol = ncols)+
              scale_x_continuous(name = "Age") + 
              scale_y_continuous(name = "Weight") +
              geom_abline(aes(intercept=Intercepts, slope=Slopes), 
                col="red", lwd=1.5, pred_model) +
              ggtitle(model_name)
  
  return(list("plot" = g, "fitted_values" = pred_model))
}

res1_rint <- plot_results(data = df_wide, model = model1_rint, 
                          model_name = "Model 1: within-person empty model",
             slopes = FALSE)
res1_rint$plot


res1_rint_fslp <- plot_results(data = df_wide, model = model1_rint_fslp, 
                               model_name = "Model 1: random intercept, fixed linear time")
res1_rint_fslp$plot


res1_rint_rslp <- plot_results(data = df_wide, model = model1_rint_rslp, 
                               model_name = "Model 1: random intercept, random linear time")
res1_rint_rslp$plot

res1_rint_rslp_quad <- plot_results(data = df_wide, model = model1_rint_rslp_quad, 
                                    model_name = "Model 1: random intercept, random quadratic time")
res1_rint_rslp_quad$plot
res1_rint_rslp_quad$fitted_values

