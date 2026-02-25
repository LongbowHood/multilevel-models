rm(list = ls())
source("data_load.R")
<<<<<<< Updated upstream
<<<<<<< Updated upstream

names(data_long)
# "ChildID"  "Age"      "Weight"   "Gender"   "NObs"     "GenderID"

=======
library(ggplot2)
>>>>>>> Stashed changes
=======
library(ggplot2)
>>>>>>> Stashed changes
verbose = FALSE


##########################################
####          Visualization           ####
##########################################

# Plot all observations as growth lines
plot_all_obs <- function(varx, vary, data, data_type,
                         transfx = "", transfy = ""){
  ggplot(data, aes(x = .data[[varx]], y = .data[[vary]])) + 
    coord_cartesian(xlim = c(min(data[varx]), max(data[varx])),
                    ylim = c(min(data[vary]), max(data[vary]))) +
    geom_line(aes(group=ChildID), col = "darkgray") + 
    geom_point(aes(fill=as.factor(ChildID)), pch=21, size=2, stroke=1) + 
    scale_x_continuous(name = paste0("Age",transfx), 
                       breaks=round(seq(min(data[varx]), max(data[varx]), length.out = 10)))  + 
    scale_y_continuous(name = paste0("Weight", transfy),
                       limits=range(data[,vary])) + 
    theme_bw() + theme(axis.text.x=element_text(size=12, colour="black"),
                       axis.text.y=element_text(size=12, colour="black"), 
                       axis.title=element_text(size=12,face="bold")) +
    theme(strip.text.x = element_text(size = 12)) + theme(legend.position="none") +
    ggtitle(data_type)
}

###

plot_facet_var <- function(varx, vary, data, data_type, var_facet,
                         transfx = "", transfy = ""){
  
  ggplot(data, aes(x = .data[[varx]], y = .data[[vary]])) + 
    coord_cartesian(xlim = c(min(data[varx]), max(data[varx])),
                    ylim = c(min(data[vary]), max(data[vary]))) +
    geom_line(aes(group=ChildID), col = "darkgray") + 
    geom_point(aes(fill=as.factor(ChildID)), pch=21, size=2, stroke=1) +
    facet_wrap(~.data[[var_facet]]) +
    scale_x_continuous(name = paste0("Age",transfx), 
                       breaks=round(seq(min(data[varx]), max(data[varx]), length.out = 6)))  + 
    scale_y_continuous(name = paste0("Weight", transfy),
                       limits=range(data[,vary])) + 
    theme_bw() + theme(axis.text.x=element_text(size=12, colour="black"),
                       axis.text.y=element_text(size=12, colour="black"), 
                       axis.title=element_text(size=12,face="bold")) +
    theme(strip.text.x = element_text(size = 12)) + theme(legend.position="none") +
    ggtitle(data_type)
}

###

# The distinct number of children in NObs groups
table(unique(data_long[, c("ChildID", "NObs")])$NObs)


if(verbose == TRUE){

<<<<<<< Updated upstream
<<<<<<< Updated upstream
  
  
## ----------- Original scale plots alone and over NObs --------------
# plot_all_obs("Age", "Weight", data_wide, "data wide")
plot_all_obs("Age", "Weight", data_long, "data long")
  
# plot_facet_var("Age", "Weight", data_wide, "data wide", "NObs")
=======
plot_all_obs("Age", "Weight", data_wide, "", 
             transfx = " (in days)", transfy = " (in grams)")
plot_all_obs("Age", "Weight", data_long, "data long")
  
plot_facet_var("Age", "Weight", data_wide, "", "NObs", 
               transfx = " (in days)", transfy = " (in grams)")
>>>>>>> Stashed changes
=======
plot_all_obs("Age", "Weight", data_wide, "", 
             transfx = " (in days)", transfy = " (in grams)")
plot_all_obs("Age", "Weight", data_long, "data long")
  
plot_facet_var("Age", "Weight", data_wide, "", "NObs", 
               transfx = " (in days)", transfy = " (in grams)")
>>>>>>> Stashed changes
plot_facet_var("Age", "Weight", data_long, "data long", "NObs")
  
}

# Rescale days to weeks and grams to kilograms to avoid calculations with large numbers
# data_wide$r_Weight <- data_wide$Weight / 1000
# data_wide$r_Birthweight <- data_wide$Birthweight / 1000
data_long$r_Weight <- data_long$Weight / 1000  # (in Kg)
 
# data_wide$r_Age_weeks <- data_wide$Age / 7 
data_long$r_Age_weeks <- data_long$Age / 7 # (in Weeks)

# data_wide$r_Age_years <- data_wide$Age / 365
data_long$r_Age_years <- data_long$Age / 365 # (in Years)

# Logarithm of the columns
# adding 1 to avoid error since 0 is there in the Age column
data_long$log_Age <- log(data_long$Age + 1) 
data_long$log_Weight <- log(data_long$Weight) 

# Standardization
data_long$n_Age <- (data_long$Age - mean(data_long$Age)) / sd(data_long$Age)
data_long$n_Weight <- (data_long$Weight - mean(data_long$Weight)) / sd(data_long$Weight)

data_long$c_Age <- (data_long$Age - mean(data_long$Age))
data_long$c_Weight <- (data_long$Weight - mean(data_long$Weight))


# data_wide$n_Age <- (data_wide$Age - mean(data_wide$Age)) / sd(data_wide$Age)
# data_wide$n_Weight <- (data_wide$Weight - mean(data_wide$Weight)) / sd(data_wide$Weight)
# data_wide$n_Birthweight <- (data_wide$Birthweight - mean(data_wide$Birthweight)) / sd(data_wide$Birthweight)
# 
# data_wide$log_Age <- log(data_wide$Age + 1)
# data_wide$log_Weight <- log(data_wide$Weight)
# data_wide$log_Birthweight <- log(data_wide$Birthweight)

# data_wide$c_Age <- (data_wide$Age - mean(data_wide$Age))
# data_wide$c_Weight <- (data_wide$Weight - mean(data_wide$Weight))
# data_wide$c_Birthweight <- (data_wide$Birthweight - mean(data_wide$Birthweight))

# data_wide$c_Age_years <- (data_wide$r_Age_years - mean(data_wide$r_Age_years))
# data_wide$c_Weight_kg <- (data_wide$r_Weight - mean(data_wide$r_Weight))
# data_wide$c_Birthweight_kg <- (data_wide$r_Birthweight - mean(data_wide$r_Birthweight))


<<<<<<< Updated upstream
# # Transformed values
# if(verbose == TRUE){
# plot_all_obs("Age", "Weight", data_wide, 
#              data_type = "data wide", transfx = " (days)", transfy = " (g)")
# plot_all_obs("r_Age", "r_Weight", data_wide, 
#              data_type = "data wide", transfx = " (years)", transfy = " (kg)")
# plot_all_obs("r_Age", "c_Weight", data_wide, 
#              data_type = "data wide", transfx = " (centered)", transfy = " (centered)")
# plot_all_obs("r_Age", "n_Weight", data_wide, 
#              data_type = "data wide", transfx = " (years)", transfy = " (norm.)")
# plot_all_obs("log_Age", "log_Weight", data_wide, 
#              data_type = "data wide", transfx = " (logarithm)", transfy = " (logarithm)")
# }
=======
# Transformed values
if(verbose == TRUE){
plot_all_obs("Age", "Weight", data_wide, 
             data_type = "data wide", transfx = " (days)", transfy = " (g)")
plot_all_obs("r_Age", "r_Weight", data_wide, 
             data_type = "data wide", transfx = " (years)", transfy = " (kg)")
plot_all_obs("r_Age", "c_Weight", data_wide, 
             data_type = "data wide", transfx = " (centered)", transfy = " (centered)")
plot_all_obs("r_Age", "n_Weight", data_wide, 
             data_type = "data wide", transfx = " (years)", transfy = " (norm.)")
plot_all_obs("log_Age", "log_Weight", data_wide, 
             data_type = "data wide", transfx = " (logarithm)", transfy = " (logarithm)")
plot_all_obs("c_Age_years", "c_Weight_kg", data_wide, 
             data_type = "", transfx = ", centered (in years)", transfy = ", centered (in kg)")
}
>>>>>>> Stashed changes

#################################################

# Exclude the data with 1 observation only (not longitudinal data)
# data_wide <- data_wide[data_wide$NObs > 1,]


if(verbose == TRUE){
  
# plot_all_obs("r_Age", "r_Weight", data_wide, 
#                data_type = "data wide", transfx = " (weeks)", transfy = " (kg)")
# plot_facet_var("r_Age", "r_Weight", data_wide, data_type = "data wide", 
#                  var_facet = "Gender", transfx = " (weeks)", transfy = " (kg)")
plot_facet_var("r_Age", "r_Weight", data_long, data_type = "data long", 
                 var_facet = "Gender", transfx = " (weeks)", transfy = " (kg)")
}

# table(data_wide$Gender, data_wide$NObs) / rep(2:5, each = 2)
table(data_long$Gender, data_long$NObs) / rep(2:6, each = 2)


names(data_long)

### ----------------------------------------------------

names(data_long)

data_long <- data_long[, c("ChildID", 
                           "Age", "log_Age","n_Age", "r_Age_weeks", "r_Age_years", "c_Age",
                           "Weight", "log_Weight", "n_Weight", "r_Weight", "c_Weight",
                           "Gender", "GenderID", "NObs")]

# Original
plot_all_obs("Age", "Weight", data_long, 
             data_type = "data long", transfx = " (days)", transfy = " (gm)")

# weight in kg vs Age in weeks
plot_all_obs("r_Age_weeks", "r_Weight", data_long, 
             data_type = "data long", transfx = " (weeks)", transfy = " (kg)")

# log weight vs Age in weeks
plot_all_obs("r_Age_weeks", "log_Weight", data_long, 
             data_type = "data long", transfx = " (weeks)", transfy = " (log_gm)")



# Group by Gender 
plot_facet_var("Age", "Weight", data_long, 
               var_facet = "Gender",
               data_type = "data long", transfx = " (days)", transfy = " (gm)")

plot_facet_var("r_Age_weeks", "r_Weight", data_long, 
               var_facet = "Gender",
               data_type = "data long", transfx = " (days)", transfy = " (gm)")

plot_facet_var("r_Age_weeks", "log_Weight", data_long,
               var_facet = "Gender",
               data_type = "data long", transfx = " (days)", transfy = " (gm)")


# group as NObs
plot_facet_var("Age", "Weight", data_long, 
               var_facet = "NObs",
               data_type = "data long", transfx = " (days)", transfy = " (gm)")

plot_facet_var("r_Age_weeks", "r_Weight", data_long, 
               var_facet = "NObs",
               data_type = "data long", transfx = " (days)", transfy = " (gm)")

plot_facet_var("r_Age_weeks", "log_Weight", data_long,
               var_facet = "NObs",
               data_type = "data long", transfx = " (days)", transfy = " (gm)")



#> log doesn't help much
#> keep Weight in kg
#> and Age in weeks
#> This should be fine, since shows the clear linear pattern with some
#> negative quadratic pattern
