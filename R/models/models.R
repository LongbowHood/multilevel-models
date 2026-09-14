library(lme4)
library(lmerTest)
library(ggplot2)

# Load data pipeline
source("R/data_prep.R")
data_wide <- get_multilevel_data(format = "wide")

# Prepare dataframe for modeling
df_wide <- data_wide[c("ChildID", "Gender", "NObs", "GenderID",
                      "c_Weight_kg", "c_Birthweight_kg", "c_Age_years")]
colnames(df_wide) <- c("ChildID", "Gender", "NObs", "GenderID",
                       "Weight", "Birthweight", "Age")

####################################
###        Model fitting         ###
####################################

#------- only age as predictor --------------------
message("Fitting model01...")
model01 <- lmer(Weight ~ 1 + (1|ChildID), data=df_wide, REML=TRUE)
message("Fitting model02...")
model02 <- lmer(Weight ~ 1 + Age + (1|ChildID), data=df_wide, REML=TRUE)
message("Fitting model03...")
model03 <- lmer(Weight ~ 1 + Age + (1 + Age|ChildID), data=df_wide, REML=TRUE)

#------- time-invariant predictors with random linear time  ----------------
message("Fitting model04...")
model04 <- lmer(Weight ~ 1 + Age + Birthweight + (1 + Age|ChildID), data=df_wide, REML=TRUE)
message("Fitting model05...")
model05 <- lmer(Weight ~ 1 + Age * Birthweight + (1 + Age|ChildID), data=df_wide, REML=TRUE)
message("Fitting model06...")
model06 <- lmer(Weight ~ 1 + Age + GenderID + (1 + Age|ChildID), data=df_wide, REML=TRUE)
message("Fitting model07...")
model07 <- lmer(Weight ~ 1 + Age * GenderID + (1 + Age|ChildID), data=df_wide, REML=TRUE)
message("Fitting model08...")
model08 <- lmer(Weight ~ 1 + Age * Birthweight + Age * GenderID + (1 + Age|ChildID), data=df_wide, REML=TRUE)

#------- time-invariant predictors with fixed linear time  ----------------
message("Fitting model14...")
model14 <- lmer(Weight ~ 1 + Age + Birthweight + (1|ChildID), data=df_wide, REML=TRUE)
message("Fitting model15...")
model15 <- lmer(Weight ~ 1 + Age * Birthweight + (1|ChildID), data=df_wide, REML=TRUE)
message("Fitting model16...")
model16 <- lmer(Weight ~ 1 + Age + GenderID + (1|ChildID), data=df_wide, REML=TRUE)
message("Fitting model17...")
model17 <- lmer(Weight ~ 1 + Age * GenderID + (1|ChildID), data=df_wide, REML=TRUE)
message("Fitting model18...")
model18 <- lmer(Weight ~ 1 + Age * Birthweight + Age * GenderID + (1|ChildID), data=df_wide, REML=TRUE)

#------- non-linear growth ------------------------
message("Fitting model09...")
model09 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1|ChildID), data=df_wide, REML=TRUE)
message("Fitting model10...")
model10 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1 + Age|ChildID), data=df_wide, REML=TRUE)
message("Fitting model11...")
model11 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1 + Age + I(Age^2)|ChildID), data=df_wide, REML=TRUE)

#------- non-linear growth with add predictors -------------
message("Fitting model12...")
model12 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1|ChildID) + Age * Birthweight + Age * GenderID, data=df_wide, REML=TRUE)
message("Fitting model13...")
model13 <- lmer(Weight ~ 1 + Age + I(Age^2) + (1 + Age|ChildID) + Age * Birthweight + Age * GenderID, data=df_wide, REML=TRUE)

#############################
#####    Save results   #####
#############################
# Just save a summary comparing models
sink("output/models/model_comparison.txt")
cat("=== ANOVA Model Comparison ===\n\n")
print(anova(model01, model02, model03))
cat("\n\n")
print(anova(model09, model10, model11))
sink()

#################################
##### Result visualization ######
#################################

time_grid <- seq(min(df_wide["Age"]), max(df_wide["Age"]), length.out = 50)

plot_results <- function(data, model, model_name, filename, n = 10, ncols = 5){
  set.seed(42)
  sample_n_id <- sample(as.vector(unique(data$ChildID[order(data$ChildID)])), n)
  sample_n_id_idx <- which(data$ChildID %in% sample_n_id)
  data_n <- data[sample_n_id_idx, c("ChildID", "Birthweight", "GenderID", "Age", "Weight")]
  unique_data_n <- unique(data_n[c("ChildID", "Birthweight", "GenderID")])
  newdata <- cbind(unique_data_n, rep(rownames(unique_data_n), each = length(time_grid)))[,1:3]
  newdata$Age <- rep(time_grid, each = n)
  newdata$Predictions <- predict(model, newdata)
  
  g = ggplot(data_n, aes(x = Age, y = Weight)) +
    geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
    geom_line(aes(group=ChildID)) + 
    facet_wrap(~ChildID, ncol=ncols)+
    scale_x_continuous(name = "Age") + 
    scale_y_continuous(name = "Weight") +
    geom_line(data = newdata, aes(x = Age, y = Predictions, group = ChildID, color = factor(ChildID)), lwd=1.5) +
    ggtitle(model_name)
  
  ggsave(filename = paste0("output/figures/", filename), plot = g, width = 10, height = 6)
  return(g)
}

message("Generating plots...")
invisible(plot_results(df_wide, model01, "Model 1: random intercept model", "model01.png"))
invisible(plot_results(df_wide, model02, "Model 2: random intercept, fixed linear time", "model02.png", n=6, ncol=3))
invisible(plot_results(df_wide, model03, "Model 3: random linear time (RLT)", "model03.png"))
invisible(plot_results(df_wide, model04, "Model 4: RLT + fixed Birthweight", "model04.png"))
invisible(plot_results(df_wide, model10, "Model 10: random linear time, fixed quadratic time", "model10.png", n=6, ncol=3))
invisible(plot_results(df_wide, model13, "Model 13: random linear time, fixed quadratic time,\nBirthweight and Gender with interactions", "model13.png", n=6, ncol=3))

message("All models fitted and plots saved successfully!")
