rm(list = ls())

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
source("cleaning_data.R")


#> 
#> Birth weight is a baseline characteristic of each child, measured at age ≈ 0 (i.e., at birth).
#> It does not change over time — it’s a level‑2 (child-level) variable.
#> So it makes sense to include it as a predictor of growth trajectory, 
#> but not as part of the repeated measures (level 1) data.




####################################
###        Model fitting         ###
####################################

# The Model fitting is done on the standardized variables 
data_3 <- data[data$NObs > 2, ] # 1199 x 13
data_4 <- data[data$NObs > 3, ] # 692 x 13


## 1) Predictors : NO -------------------------------

# random-intercept-only model
(model01 <- lmer(Weight_sd ~ 
                   # fixed effect : Intercept
                   1 + 
                   # random effect : Intercept
                   (1|ChildID), 
                 data=data, REML=TRUE))

#> there’s no detectable between‑child variability in average 
#> standardized weights.
#> After standardizing weight values, children do not differ systematically 
#> in their average standardized weight; 
#> all observed variation is within-child over time or measurement noise.

## 2) Predictors : Age -------------------------------

(model02 <- lmer(Weight_sd ~ 
                   # fixed effect : Intercept + Age
                   1 + Age_sd +
                   # random effect : Intercept
                   (1|ChildID), 
                 data=data, REML=TRUE))

#> for each one standard deviation increase in age, the standardized weight 
#> increases by about 0.885 SD units.
#> older children are heavier relative to the population mean.
#> 
#> There is measurable between-child variability
#> A child one SD older tends to weigh about 0.9 SD more than a younger peer 
#> on average, and individual baselines differ by ±~¼ SD around that trend.
#> 




(model03 <- lmer(Weight_sd ~ 
                   # fixed effect : Intercept + Age
                   1 + Age_sd +
                   # random effect : Intercept + Age
                   (1 + Age_sd|ChildID), 
                 data=data, REML=TRUE))
#>
#> For each one SD increase in age, standardized weight increases by 
#> about +0.90 SD units — very similar to your previous model’s estimate
#> 
#> Children differ in baseline standardized weight by ±~¼ SD
#> Individual growth rates vary slightly around the average 
#> of +0.90 SD per age SD
#> Correlation between intercept and slope = +1
#> 
#> The perfect correlation (+1) between random intercept and random slope means 
#> that once the model knows a child’s baseline deviation (u0iu_{0i}u0i), 
#> it can perfectly predict their slope deviation (u1iu_{1i}u1i).
#> 





(model04 <- lmer(Weight_sd ~ 
                   # fixed effect : Intercept + Age
                   1 + Age_sd +
                   # random effect : Intercept + Age
                   (1 + Age_sd||ChildID), # No correlation bw Incpt and Slope
                 data=data, REML=TRUE))

#> the model concludes that all children follow essentially the same 
#> standardized age–weight trend; only their average position on that 
#> line differs slightly.
#> Growth patterns are uniform across individuals once both weight and age 
#> are expressed as standardized scores; only small baseline differences 
#> remain between children.



## 3) Predictors : Age + Birthweight ------------------------

(model05 <- lmer(Weight_sd ~ 
                   # fixed effect : Intercept + Age + Birthweight
                   1 + Age_sd + Birthweight_sd +
                   # random effect : Intercept
                   (1|ChildID), 
                 data=data, REML=TRUE))



#> So children who were heavier at birth tend to remain slightly heavier 
#> relative to others over time.



#> does Birthweight vary within a child?
#> No — each child has only one birth weight value.




## 4) Predictors : Age + Birthweight + Gender ------------------------


(model06 <- lmer(Weight_sd ~ 
                   # fixed effect : Intercept + Age + Birthweight + Gender
                   1 + Age_sd + Birthweight_sd + Gender +
                   # random effect : Intercept 
                   (1|ChildID), 
                 data=data, REML=TRUE))


#> It explains growth through age, initial size through birth weight, 
#> sex differences through gender, and allows each child a unique baseline 
#> via a random intercept.
#> 




# 5) Predictors : non-linear growth over Age ------------------------
(model07 <- lmer(Weight_sd ~ 
                   # fixed effect : Intercept + Age + Birthweight + Gender
                   1 + Age_sd + I(Age_sd^2) + Birthweight_sd + Gender +
                   # random effect : Intercept
                   (1 |ChildID), 
                 data=data, REML=TRUE))
#> Negative curvature: growth slows down at higher ages; the rate of increase 
#> flattens out over time — typical pattern in early childhood growth 
#> where rapid initial gains taper off later.
#> 


#> Your fixed effects confirm a realistic nonlinear growth curve with 
#> expected birth‑size and gender influences. 
#> The singularity warning simply means that estimating separate random slopes 
#> for both Age and Age² pushes beyond what your data can reliably support.




# Best one
(model.long.5 <- lmer(n_Weight ~ 1 + n_Age + 
                        (1 | ChildID), 
                      data = data_long))

(model.long.6 <- lmer(n_Weight ~ 1 + n_Age + 
                        (1 + n_Age | ChildID), 
                      data = data_long))

(model.long.6 <- lmer(n_Weight ~ 1 + n_Age + 
                        (1 + n_Age || ChildID), 
                      data = data_long))

#############################
#####    Save results   #####
#############################

df_result = data

df_result["m01_fitted"] <- fitted(model01)
df_result["m02_fitted"] <- fitted(model02)
df_result["m03_fitted"] <- fitted(model03)
df_result["m04_fitted"] <- fitted(model04)
df_result["m05_fitted"] <- fitted(model05)
df_result["m06_fitted"] <- fitted(model06)
df_result["m07_fitted"] <- fitted(model07)


#################################
##### Result visualization ######
#################################

time_grid_sd <- seq(min(data["Age_sd"]), max(data["Age_sd"]), length.out = 50)

plot_results <- function(data, model, model_name,
                         n = 10, ncols = 5){
  
  set.seed(42)
  
  # Sample children
  sample_n_id <- sample(unique(data$ChildID), n)
  
  # Observed data for those children
  data_n <- data[data$ChildID %in% sample_n_id, ]
  
  # Create prediction grid (n children × 50 time points)
  newdata <- expand.grid(
    ChildID = sample_n_id,
    Age_sd  = time_grid_sd
  )
  
  # Add predictions
  newdata$Predictions <- predict(model, newdata)
  
  # Plot
  g <- ggplot(data_n, aes(x = Age_sd, y = Weight_sd)) +
    geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
    geom_line(aes(group=ChildID)) +
    facet_wrap(~ChildID, ncol=ncols) +
    geom_line(data = newdata,
              aes(x = Age_sd, y = Predictions, group = ChildID),
              linewidth=1.2, color="blue") +
    ggtitle(model_name)
  
  return(g)
}




plot_results(data, model01, 
             model_name = "Model 1: within-person empty model")
plot_results(data, model02, 
             model_name = "Model 2: random intercept, fixed linear time")
plot_results(data, model03, 
             model_name = "Model 3: random linear time (RLT)")
plot_results(data, model04, 
             model_name = "Model 4: RLT + fixed Birthweight")
plot_results(data, model05, 
             model_name = "Model 5: RLT + fixed Birthweight with interaction")
plot_results(data, model06, 
             model_name = "Model 6: RLT + fixed Gender")
plot_results(data, model07, 
             model_name = "Model 7: RLT + fixed Birthweight and Gender")