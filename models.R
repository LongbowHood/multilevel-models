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
df5_long <- df5_long[,-6]
df5_wide <- data_wide[data_wide$NObs == 4,]
df5_wide <- df5_wide[,-9]

# Exclude the data with 1 observation only (not longitudinal data)
data_wide <- data_wide[data_wide$NObs > 1,]


# Dataset ---------------------------------------------------
# Select only relevant columns
df5_long_0 <- df5_long[ , c("ChildID", "Age", "Weight")]

# Standardization
mean(df5_long_0$Age)  # 298.7524  ~ 300
sd(df5_long_0$Age)    # 292.6621 ~ 300
range(df5_long_0$Age) # 0 948

df5_long_0$Age_std <- (df5_long_0$Age - 300) / 300
range(df5_long_0$Age_std) 



mean(df5_long_0$Weight)  # 7847.229 ~ 8000
sd(df5_long_0$Weight)    # 3705.581 ~ 4000
range(df5_long_0$Weight) #  1630 17300

df5_long_0$Weight_std <- (df5_long_0$Weight - 8000) / 4000
range(df5_long_0$Weight_std)


df5_long_0_std <- df5_long_0[, c("ChildID", "Age_std", "Weight_std")]



ggplot(df5_long_0_std, aes(x = Age_std, y = Weight_std)) + 
  # Observed lines per child
  geom_line(aes(group = ChildID), color = "grey70") + 
  # Observed points
  geom_point(aes(fill = as.factor(ChildID)), pch = 21, size = 2, stroke = 1) + 
  # Axes
  scale_x_continuous(name = "Standardized Age", breaks = seq(-1, 2.5, 0.5)) + 
  scale_y_continuous(name = "Standardized Weight", limits = c(-1.5, 3)) + 
  # Theme
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black"), 
    axis.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 12),
    legend.position = "none"
  )

head(df5_long_0_std)

# 1) Fixed intercept + Random Intercept ----------------------------------------

model_int_std <- lmer(Weight_std ~ 1 + (1|ChildID), data=df5_long_0_std, REML=FALSE)

summary(model_int_std)

df5_long_0_std$pred_child_std <- predict(model_int_std)

df5_long_0_std$pred_child






# 2) Random Intercept + Fixed slope  --------------------------

model_int_fslp_std <- lmer(Weight_std ~ 1 + Age_std + (1|ChildID), data=df5_long_0_std, REML=FALSE)

summary(model_int_fslp_std)
df5_long_0_std$pred_child_fslp <- predict(model_int_fslp_std)

ggplot(df5_long_0_std, aes(x = Age, y = Weight)) + 
  # Observed lines per child
  geom_line(aes(group = ChildID), color = "grey70") + 
  # Observed points
  geom_point(aes(fill = as.factor(ChildID)), pch = 21, size = 2, stroke = 1) + 
  # Predicted line per child
  geom_line(aes(y = pred_child_fslp, group = ChildID, color = as.factor(ChildID)), size = 1) +
  # Axes
  scale_x_continuous(name = "Age", breaks = seq(0, 980, 200)) + 
  scale_y_continuous(name = "Weight", limits = c(950, 20000)) + 
  # Theme
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black"), 
    axis.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 12),
    legend.position = "none"
  )







# 3) Random slope + Random slope   --------------------------------------------

model_int_slope_std <- lmer(Weight_std ~ 1 + Age_std + (1 + Age_std | ChildID), data=df5_long_0_std, REML=FALSE)
model_int_slope_std_uncorr <- lmer(Weight_std ~ 1 + Age_std + (1 + Age_std || ChildID), data=df5_long_0_std, REML=FALSE)

isSingular(model_int_slope_std)
isSingular(model_int_slope_std_uncorr)

VarCorr(model_int_slope_std)
VarCorr(model_int_slope_std_uncorr)



## The slope and intercept are linearly dependent
# even removing the correlation does not help

summary(model_int_slope_std)














# Model fitting (trial and error)

# wide data

model_w_rint <- lmer(n_Weight ~ 1 + (1|ChildID), 
                     data=df5_wide, REML=FALSE)
### -> didn't fit

model_w_rint_fslp <- lmer(n_Weight ~ 1 + n_Age + (1|ChildID),
                          data=df5_wide, REML=FALSE)

model_w_rint_rslp <- lmer(n_Weight ~ 1 + n_Age + (1 + n_Age|ChildID),
                          data=df5_wide, REML=FALSE)

### -> didn't fit

# long data

model_l_rint <- lmer(log_Weight ~ 1 + (1|ChildID), 
                     data=df5_long, REML=FALSE)
### -> didn't fit

model_l_rint_fslp <- lmer(log_Weight ~ 1 + log_Age + (1|ChildID),
                          data=df5_long, REML=FALSE)

model_l_rint_rslp <- lmer(log_Weight ~ 1 + log_Age + (1 + log_Age|ChildID) ,
                          data=df5_long, REML=FALSE)
### -> didn't fit


##Result visualization
first10_w_id <- unique(df5_wide$ChildID[order(df5_wide$ChildID)])
first10_w <- df5_wide[df5_wide$ChildID %in% first10_w_id,]


pred_w_rint_fslp <-data.frame(ChildID=unique(df5_wide$ChildID[order(df5_wide$ChildID)])[1:10], 
                              Intercepts=c(coef(model_w_rint_fslp)$ChildID[c(1:10),1]), 
                              Slopes=c(coef(model_w_rint_fslp)$ChildID[c(1:10),2])) 

ggplot(first10_w, aes(x = n_Age, y = n_Weight)) + 
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  geom_line(aes(group=ChildID)) + facet_wrap(~ChildID, ncol=5)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100))+
  geom_abline(aes(intercept=Intercepts, slope=Slopes), col="red", lwd=1.5, pred_w_rint_fslp)
