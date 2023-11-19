library(ggplot2)

food_data <- read.csv("Data/foodtrap.csv")
food_data <- food_data[,1:3]

split_food <- split(food_data, food_data$Genotype)

# Control
food_data_c <- split_food$C
split_c <- split(food_data_c, food_data_c$Food..F..or.agar..A.)
split_c[[1]] <- split_c[[1]][[3]]
split_c[[2]] <- split_c[[2]][[3]]

agar_df <- data.frame("controla" = split_c[[1]])
food_df <- data.frame("controlf" = split_c[[2]])

# Mutant 1
food_data_m1 <- split_food$M1
split_m1 <- split(food_data_m1, food_data_m1$Food..F..or.agar..A.)
split_m1[[1]] <- split_m1[[1]][[3]]
split_m1[[2]] <- split_m1[[2]][[3]]

agar_df$mutant1a <- split_m1[[1]]
food_df$mutant1f <- split_m1[[2]]

# Mutant 2
food_data_m2 <- split_food$M2
split_m2 <- split(food_data_m2, food_data_m2$Food..F..or.agar..A.)
split_m2[[1]] <- c (0, 0, 0, 0, 0)
split_m2[[2]] <- split_m2[[2]][[3]]

agar_df$mutant2a <- split_m2[[1]]
food_df$mutant2f <- split_m2[[2]]

mean_a <- apply(agar_df, 2, mean) # mean
sd_a <- apply(agar_df, 2, sd) # sd
se_a <- (sd_a)/sqrt(5) # se

mean_f <- apply(food_df, 2, mean) # mean
sd_f <- apply(food_df, 2, sd) # sd
se_f <- (sd_f)/sqrt(5) # se

# Processing agar and food data sets
agar_df["mean",] <- mean_a
agar_df["sd",] <- sd_a
agar_df["se",] <- se_a
agar_df["genotype",] <- c("control", "mutant1", "mutant2")
agar_df["test",] <- c("agar", "agar", "agar")

food_df["mean",] <- mean_f
food_df["sd",] <- sd_f
food_df["se",] <- se_f
food_df["genotype",] <- c("control", "mutant1", "mutant2")
food_df["test",] <- c("food", "food", "food")

# Combining all in to one data frame so that it can be plotted on one graph
plot_df <- merge(agar_df, food_df, by = "row.names", all = TRUE)
rownames(plot_df) <- plot_df$Row.names
plot_df$Row.names <- NULL

update_df <- data.frame("id" = c("ca", "m1a", "m2a", "cf", "m2f", "m2f"),
                        "genotype" = as.character(plot_df["genotype",]), 
                        "test" = as.character(plot_df["test",]),
                        "mean" = as.character(plot_df["mean",]),
                        "se" = as.numeric(plot_df["se",])
)
update_df$mean <- as.numeric(update_df$mean)


# Plot--------------------------------------------------------------------------
foodtrap_plot <-
  ggplot(data = update_df, aes(x = genotype, y = mean, fill = test)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5) + 
  scale_fill_manual(values=c("red4", "cornflowerblue")) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.1),
                position=position_dodge(.5)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("Control", "Mutant 1", "Mutant 2")) + 
  xlab("Genotype") + ylab("Mean Number of Flies in Trap") + labs(fill = "Trap") +
  theme_bw()

#ggsave(plot = foodtrap_plot, filename = "Graphs/food trap plot.png", 
 #      width = 6.25, height = 5)

# Significance analysis---------------------------------------------------------
# Normality test
shapiro.test(food_df$controlf) # 0.814 normal
shapiro.test(food_df$mutant1f ) # 0.000131 not normal
shapiro.test(food_df$mutant2f) # 0.4211 normal

# Variance test
var.test(food_df$controlf, food_df$mutant1f, alternative = "two.sided") 
 # 0.2203 equal variance

var.test(food_df$controlf, food_df$mutant2f, alternative = "two.sided") 
# 0.7647059  equal variance

# T-test
t.test(food_df$controlf, food_df$mutant1f, alternative = "two.sided",
       mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
# 0.000142 significant difference

t.test(food_df$controlf, food_df$mutant2f, alternative = "two.sided",
       mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
# 0.3319 non-significant difference
