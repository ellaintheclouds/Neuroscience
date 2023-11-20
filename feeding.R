# Set up------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)


# Standard curve----------------------------------------------------------------
standard_data <- read.csv("Data/feedingstandard.csv")
standard_data$absorbance <- standard_data$absorbance - 0.032

# Creating a model
model4pl <- function(concentrationμg , Background, Mid, Slope, Bmax) {
  Bmax + ((Background - Bmax) / (1 + ((concentrationμg /Mid)^Slope)))
}

# Iteration
fit <- nls(absorbance ~ model4pl(concentrationμg , Background, Mid, Slope, Bmax),
           data = standard_data,
           start = c(Background=0, Mid=17, Slope=1, Bmax=2.4),
           control = nls.control(maxiter=1000, warnOnly=TRUE))
print(fit)

cor(standard_data$absorbance, predict(fit))

# Plot
standard_curve <- ggplot(standard_data, aes(x = concentrationμg, y = absorbance)) + 
  geom_point() + scale_x_log10() +
  stat_function(data = standard_data, fun  = model4pl, colour = "cornflowerblue",
                args = list(Mid = coef(fit)["Mid"],
                            Background = coef(fit)["Background"],
                            Slope = coef(fit)["Slope"],
                            Bmax = coef(fit)["Bmax"])) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  xlab("Log(Food Concentration (μgml-1))") + ylab("Absorbance Units (450 nm)") +
  theme_bw()

#ggsave("Graphs/feeding standard curve modelled.png", plot = standard_curve, 
 #       width = 6.25, height = 5)


# Class data processing---------------------------------------------------------
data <- read.csv("Data/feeding.csv")
data <- data[,1:3]

# Adding a column with the mean value for normal food subtracted
data$blank_subtracted <- data$Spec.Reading - 0.07

# Adding a column that uses this in the equation gained from standard curve
# to determine food concentration in μg mL-1
CalcConc <- function(Background, Mid, Slope, Bmax, y) {
  as.numeric(Mid * ((Background - Bmax)/(y - Bmax) - 1)^(1/Slope))
}

data$conc_calculated <- CalcConc(coef(fit)["Background"],
                                  coef(fit)["Mid"],
                                  coef(fit)["Slope"],
                                  coef(fit)["Bmax"],
                                  y = data$blank_subtracted)

# Replace NaN with 0
data <- data %>% mutate(conc_calculated = ifelse(is.na(conc_calculated), 
                                                         0, conc_calculated))

# Splitting in to genotype and food type for comparison
split_genotype <- split(data, data$Genotype)

split_food_c <- split(split_genotype$Control, 
                      split_genotype$Control$Blue.Food.Type)
c_low <- split_food_c$Low
c_high <- split_food_c$High

split_food_m1 <- split(split_genotype$M1, 
                      split_genotype$M1$Blue.Food.Type)
m1_low <- split_food_m1$Low
m1_high <- split_food_m1$High

split_food_m2 <- split(split_genotype$M2, 
                       split_genotype$M2$Blue.Food.Type)
m2_low <- split_food_m2$Low
m2_high <- split_food_m2$High

# Mean and sd
c_low_mean <- mean(c_low$conc_calculated)
c_low_sd <- sd(c_low$conc_calculated)
c_low_se <- (c_low_sd)/sqrt(16)

c_high_mean <- mean(c_high$conc_calculated)
c_high_sd <- sd(c_high$conc_calculated)
c_high_se <- (c_high_sd)/sqrt(16)

m1_low_mean <- mean(m1_low$conc_calculated)
m1_low_sd <- sd(m1_low$conc_calculated)
m1_low_se <- (m1_low_sd)/sqrt(16)

m1_high_mean <- mean(m1_high$conc_calculated)
m1_high_sd <- sd(m1_high$conc_calculated)
m1_high_se <- (m1_high_sd)/sqrt(16)

m2_low_mean <- mean(m2_low$conc_calculated)
m2_low_sd <- sd(m2_low$conc_calculated)
m2_low_se <- (m2_low_sd)/sqrt(16)

m2_high_mean <- mean(m2_high$conc_calculated)
m2_high_sd <- sd(m2_high$conc_calculated)
m2_high_se <- (m2_high_sd)/sqrt(17)


# Plot--------------------------------------------------------------------------
plot_df <- data.frame("id" = c("cl", "ch", "m1l", "m1h", "m2l", "m2h"),
                        "genotype" = c("c", "c", "m1", "m1", "m2", "m2"), 
                        "food" = c("low", "high", "low", "high", "low", "high"),
                        "mean" = c(c_low_mean, c_high_mean, 
                                   m1_low_mean, m1_high_mean, 
                                   m2_low_mean, m2_high_mean),
                        "se" = c(c_low_se, c_high_se, 
                                 m1_low_se, m1_high_se, 
                                 m2_low_se, m2_high_se)
)

feeding_plot <-
  ggplot(data = plot_df, aes(x = genotype, y = mean, fill = food)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5) + 
  scale_fill_manual(values=c("red4", "cornflowerblue")) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.1),
                position=position_dodge(.5)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("Control", "Mutant 1", "Mutant 2")) + 
  xlab("Genotype") + ylab("Mean Food Quantity (μg mL-1)") + 
  labs(fill = "Food Quality") +
  theme_bw()

ggsave(plot = feeding_plot, filename = "Graphs/feeding plot.png", 
       width = 6.25, height = 5)


# Significance------------------------------------------------------------------
var.test(c_low$conc_calculated, c_high$conc_calculated, alternative = "greater") 
# 0.03287 unequal variance

var.test(m1_low$conc_calculated, m1_high$conc_calculated, alternative = "greater") 
# 6.175e-07 unequal variance

var.test(m2_low$conc_calculated, m2_high$conc_calculated, alternative = "greater") 
# 0.05441 equal variance

t.test(c_low$conc_calculated, c_high$conc_calculated, alternative = "greater", 
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
# 4.409e-05 significant

t.test(m1_low$conc_calculated, m1_high$conc_calculated, alternative = "greater", 
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
# 0.00154 significant

t.test(m2_low$conc_calculated, m2_high$conc_calculated, alternative = "greater", 
       mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
# 7.471e-06 significant