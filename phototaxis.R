library(ggplot2)

phototaxis_data <- read.csv("Data/phototaxis.csv")
phototaxis_data <- phototaxis_data[1:116, 1:7]
colnames(phototaxis_data) <- c("genotype", "1min", "2min", "3min", "4min", 
                               "5min", "phototaxis_score")

split <- split(phototaxis_data, phototaxis_data$genotype)

phototaxis_data_c <- split$C
phototaxis_data_c <- phototaxis_data_c[, 2:7]
mean_c <- mean(phototaxis_data_c$phototaxis_score)
sd_c <- sd(phototaxis_data_c$phototaxis_score)
se_c <- (sd_c)/sqrt(38) # se


phototaxis_data_m1 <- split$M1
phototaxis_data_m1 <- phototaxis_data_m1[, 2:7]
mean_m1 <- mean(phototaxis_data_m1$phototaxis_score)
sd_m1 <- sd(phototaxis_data_m1$phototaxis_score)
se_m1 <- (sd_m1)/sqrt(39) # se

phototaxis_data_m2 <- split$M2
phototaxis_data_m2 <- phototaxis_data_m2[, 2:7]
mean_m2 <- mean(phototaxis_data_m2$phototaxis_score)
sd_m2 <- sd(phototaxis_data_m2$phototaxis_score)
se_m2 <- (sd_m2)/sqrt(39) # se

# Plot--------------------------------------------------------------------------
plot_df <- data.frame("genotype" = c("Control", "Mutant 1", "Mutant 2"),
                      "mean" = c(mean_c, mean_m1, mean_m2), 
                      "se" = c(se_c, se_m1, se_m2))

phototaxis_plot <- 
  ggplot(plot_df, aes(x = genotype, y = mean)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.1)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  theme_bw() + 
  xlab("Genotype") + ylab("Mean Phototaxis Score")

ggsave(plot = phototaxis_plot, filename = "Graphs/phototaxis plot.png", 
       width = 6.25, height = 5)

# Significance------------------------------------------------------------------
# Normality test 
shapiro.test(phototaxis_data_c$phototaxis_score) # 0.008909 not normal
shapiro.test(phototaxis_data_m1$phototaxis_score) # 0.1755 normal
shapiro.test(phototaxis_data_m2$phototaxis_score) # 0.02243 not normal

# Significance Test
wilcox_c_m1 <- wilcox.test(phototaxis_data_c$phototaxis_score, 
                           phototaxis_data_m1$phototaxis_score, 
                           alternative = "two.sided")
wilcox_c_m1$p.value # 0.8135318 not significant

wilcox_c_m2 <- wilcox.test(phototaxis_data_c$phototaxis_score, 
                           phototaxis_data_m2$phototaxis_score, 
                           alternative = "two.sided")
wilcox_c_m2$p.value # 0.766309 not significant
