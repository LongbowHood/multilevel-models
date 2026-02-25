source("loading_data.R")

names(data)
# "ChildID"  "Age"      "Weight"   "Gender"   "NObs"     "GenderID"

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
    geom_line(aes(group=ChildID)) + 
    geom_point(aes(fill=as.factor(ChildID)), pch=21, size=2, stroke=1) + 
    scale_x_continuous(name = paste0("Age",transfx), 
                       breaks=round(seq(min(data[varx]), max(data[varx]), length.out = 10)))  + 
    scale_y_continuous(name = paste0("Weight", transfy),
                       limits=range(data[,vary])) + 
    theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                       axis.text.y=element_text(size=10, colour="black"), 
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
    geom_line(aes(group=ChildID)) + 
    geom_point(aes(fill=as.factor(ChildID)), pch=21, size=2, stroke=1) +
    facet_wrap(~.data[[var_facet]]) +
    scale_x_continuous(name = paste0("Age",transfx), 
                       breaks=round(seq(min(data[varx]), max(data[varx]), length.out = 10)))  + 
    scale_y_continuous(name = paste0("Weight", transfy),
                       limits=range(data[,vary])) + 
    theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                       axis.text.y=element_text(size=10, colour="black"), 
                       axis.title=element_text(size=12,face="bold")) +
    theme(strip.text.x = element_text(size = 12)) + theme(legend.position="none") +
    ggtitle(data_type)
}

# The distinct number of children in NObs groups
table(unique(data[, c("ChildID", "NObs")])$NObs)
#  1   2   3   4   5
# 91 141 169 143  24 



## ----------- Original scale plots alone and over NObs --------------

# randomly select 10 children
set.seed(4)
ten_children_ids <- sample(unique(data$ChildID), size = 10)

# Filter data_long for the 10 selected children
ten_children <- data %>% 
  filter(ChildID %in% ten_children_ids)

plot_all_obs("Age", "Weight", data, "data long")
plot_all_obs("Age", "Weight", ten_children, "data long")

plot_facet_var("Age", "Weight", data, "data long", "NObs")
plot_facet_var("Age", "Weight", ten_children, "data long", "NObs")
  

# Scaling wrt measurement units
data$Weight_kg <- data$Weight / 1000  # (gm --> kg)
data$Birthweight_kg <- data$Birthweight/1000 # (gm --> kg)
data$Age_week <- data$Age / 7 # (Days --> week)


# Standardization
data$Weight_sd <- (data$Weight - mean(data$Weight)) / sd(data$Weight)
data$Birthweight_sd <- (data$Birthweight - mean(data$Birthweight)) / sd(data$Birthweight)
data$Age_sd <- (data$Age - mean(data$Age)) / sd(data$Age)

# Exclude the data with 1 observation only (not longitudinal data)
data <- data[data$NObs > 1, ] # 1481 x 13

table(data$Gender, data$NObs) / rep(2:5, each = 2)

length(unique(data$ChildID)) # 477 unique children


### ---------- Cleaned dataset -----------------------------

# randomly select 10 children
set.seed(4)
ten_children_ids <- sample(unique(data$ChildID), size = 10)

# Filter data_long for the 10 selected children
ten_children <- data %>% 
  filter(ChildID %in% ten_children_ids)



# ------------ Original scales ---------------
plot_all_obs("Age", "Weight", data, data_type = "data long", 
             transfx = " (days)", transfy = " (gm)")

# weight in kg vs Age in weeks
plot_all_obs("Age_week", "Weight_kg", data, data_type = "data long", 
             transfx = " (week)", transfy = " (kg)")

# 
plot_all_obs("Age_sd", "Weight_sd", data, data_type = "data long", 
             transfx = " (sd)", transfy = " (sd)")



# ------------- facet by Gender -------------------
plot_facet_var("Age", "Weight", data, data_type = "data long", 
               var_facet = "Gender", 
               transfx = " (days)", transfy = " (gm)")

plot_facet_var("Age_week", "Weight_kg", data, data_type = "data long",
               var_facet = "Gender", 
               transfx = " (week)", transfy = " (kg)")

plot_facet_var("Age_sd", "Weight_sd", data, data_type = "data long",
               var_facet = "Gender", 
               transfx = " (sd)", transfy = " (sd)")


# -------------- facet as NObs ----------------------
plot_facet_var("Age", "Weight", data, data_type = "data long", 
               var_facet = "NObs", 
               transfx = " (days)", transfy = " (gm)")

plot_facet_var("Age_week", "Weight_kg", data, data_type = "data long",
               var_facet = "NObs", 
               transfx = " (week)", transfy = " (kg)")

plot_facet_var("Age_sd", "Weight_sd", data, data_type = "data long",
               var_facet = "NObs", 
               transfx = " (sd)", transfy = " (sd)")


## ============== 10 Random students ================
# randomly select 10 children
set.seed(4)
ten_children_ids <- sample(unique(data$ChildID), size = 10)

# Filter data_long for the 10 selected children
ten_children <- data %>% 
  filter(ChildID %in% ten_children_ids)



# ------------ Original scales ---------------
plot_all_obs("Age", "Weight", ten_children, data_type = "data long", 
             transfx = " (days)", transfy = " (gm)")

# weight in kg vs Age in weeks
plot_all_obs("Age_week", "Weight_kg", ten_children, data_type = "data long", 
             transfx = " (week)", transfy = " (kg)")

# 
plot_all_obs("Age_sd", "Weight_sd", ten_children, data_type = "data long", 
             transfx = " (sd)", transfy = " (sd)")



# ------------- facet by Gender -------------------
plot_facet_var("Age", "Weight", ten_children, data_type = "data long", 
               var_facet = "Gender", 
               transfx = " (days)", transfy = " (gm)")

plot_facet_var("Age_week", "Weight_kg", ten_children, data_type = "data long",
               var_facet = "Gender", 
               transfx = " (week)", transfy = " (kg)")

plot_facet_var("Age_sd", "Weight_sd", ten_children, data_type = "data long",
               var_facet = "Gender", 
               transfx = " (sd)", transfy = " (sd)")


# -------------- facet as NObs ----------------------
plot_facet_var("Age", "Weight", ten_children, data_type = "data long", 
               var_facet = "NObs", 
               transfx = " (days)", transfy = " (gm)")

plot_facet_var("Age_week", "Weight_kg", ten_children, data_type = "data long",
               var_facet = "NObs", 
               transfx = " (week)", transfy = " (kg)")

plot_facet_var("Age_sd", "Weight_sd", ten_children, data_type = "data long",
               var_facet = "NObs", 
               transfx = " (sd)", transfy = " (sd)")


data


#> log doesn't help much
#> keep Weight in kg
#> and Age in weeks
#> This should be fine, since shows the clear linear pattern with some
#> negative quadratic pattern maybe
