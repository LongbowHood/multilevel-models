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

# data exploration
pairs(df5_long[,-1])

ggplot(data_wide, aes(x = Age, y = Weight)) + 
  geom_line(aes(group=ChildID)) + 
  geom_point(aes(fill=as.factor(ChildID)), pch=21, size=1, stroke=1) + 
  facet_wrap(~NObs) +
  scale_x_continuous(name = "Age in days", breaks=seq(0,980, 200)) + 
  scale_y_continuous(name = "Weight in grams",limits=c(950,20000)) + 
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")

ggplot(data_long, aes(x = Age, y = Weight)) + 
  geom_line(aes(group=ChildID)) + 
  geom_point(aes(fill=as.factor(ChildID)), pch=21, size=1, stroke=1) + 
  facet_wrap(~NObs) +
  scale_x_continuous(name = "Age in days", breaks=seq(0,980, 200)) + 
  scale_y_continuous(name = "Weight in grams",limits=c(950,20000)) + 
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")

# Exclude the data with 1 observation only (not longitudinal data)
data_wide <- data_wide[data_wide$NObs > 1,]

# or with only 2 observations from very different time points


# Our data 
ggplot(df5_long, aes(x = log_Age, y = log_Weight)) + 
  geom_line(aes(group=ChildID)) + 
  geom_point(aes(fill=as.factor(ChildID)), pch=21, size=2, stroke=1) + 
  scale_x_continuous(name = "log(Age)", breaks=seq(0, 7, 0.5)) + 
  scale_y_continuous(name = "log(Weight)",limits=c(6,10)) + 
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")

ggplot(df5_long, aes(x = Age, y = Weight)) + 
  geom_line(aes(group=ChildID)) + 
  geom_point(aes(fill=as.factor(ChildID)), pch=21, size=2, stroke=1) + 
  scale_x_continuous(name = "Age", breaks=seq(0, 980, 200)) + 
  scale_y_continuous(name = "Weight",limits=c(950, 20000)) + 
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")

ggplot(df5_wide, aes(x = n_Age, y = n_Weight)) + 
  geom_line(aes(group=ChildID)) + 
  geom_point(aes(fill=as.factor(ChildID)), pch=21, size=2, stroke=1) + 
  scale_x_continuous(name = "Age in days (normalized)", breaks=seq(-2, 2, 0.2)) + 
  scale_y_continuous(name = "Weight in grams (normalized)",limits=c(-3, 3)) + 
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")

#################################################

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
first_w_id <- as.vector(unique(df5_wide$ChildID[order(df5_wide$ChildID)][1:20]))
first_w <- df5_wide[df5_wide$ChildID %in% first_w_id,]


pred_w_rint_fslp <-data.frame(ChildID=unique(df5_wide$ChildID[order(df5_wide$ChildID)])[1:5], 
                              Intercepts=c(coef(model_w_rint_fslp)$ChildID[c(1:5),1]), 
                              Slopes=c(coef(model_w_rint_fslp)$ChildID[c(1:5),2])) 

ggplot(first_w, aes(x = n_Age, y = n_Weight)) + 
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  geom_line(aes(group=ChildID)) + facet_wrap(~ChildID, ncol=5)+
  scale_x_continuous(name = "Age") + 
  scale_y_continuous(name = "Weight",limits=c(-3,3))+
  geom_abline(aes(intercept=Intercepts, slope=Slopes), col="red", lwd=1.5, pred_w_rint_fslp)

first_l_id <- as.vector(unique(df5_long$ChildID[order(df5_long$ChildID)][1:20]))
first_l <- df5_long[df5_long$ChildID %in% first_l_id,]


pred_l_rint_fslp <-data.frame(ChildID=unique(df5_long$ChildID[order(df5_long$ChildID)])[1:5], 
                              Intercepts=c(coef(model_l_rint_fslp)$ChildID[c(1:5),1]), 
                              Slopes=c(coef(model_l_rint_fslp)$ChildID[c(1:5),2])) 

ggplot(first_l, aes(x = log_Age, y = log_Weight)) + 
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  geom_line(aes(group=ChildID)) + facet_wrap(~ChildID, ncol=5)+
  scale_x_continuous(name = "log_Age") + 
  scale_y_continuous(name = "log_Weight",limits=c(0,10))+
  geom_abline(aes(intercept=Intercepts, slope=Slopes), col="red", lwd=1.5, pred_w_rint_fslp)

