library(ggplot2)

olfaction_data <- read.csv("Data/olfaction.csv")
olfaction_data <- olfaction_data[1:105,1:2]

split_olf <- split(olfaction_data, olfaction_data$Genotype)

olfaction_data_c <- split_olf$C
olfaction_data_c <- olfaction_data_c[,2]
olf_mean_c <- mean(olfaction_data_c) # mean
olf_sd_c <- sd(olfaction_data_c) # sd
olf_se_c <- (olf_sd_c)/sqrt(35) # se

olfaction_data_m1 <- split_olf$M1
olfaction_data_m1 <- olfaction_data_m1[,2]
olf_mean_m1 <- mean(olfaction_data_m1) # mean
olf_sd_m1 <- sd(olfaction_data_m1) # sd
olf_se_m1 <- (olf_sd_m1)/sqrt(35) # se

olfaction_data_m2 <- split_olf$M2
olfaction_data_m2 <- olfaction_data_m2[,2]
olf_mean_m2 <- mean(olfaction_data_m2) # mean
olf_sd_m2 <- sd(olfaction_data_m2) # sd
olf_se_m2 <- (olf_sd_m2)/sqrt(35) # se

olf_plot_df <- data.frame(
  "Genotype" = c("Control", "Mutant 1", "Mutant 2"), 
  "Avoidance Score" = c(olf_mean_c, olf_mean_m1, olf_mean_m2), 
  "se" = c(olf_se_c, olf_se_m1, olf_se_m2)
)

# Plot--------------------------------------------------------------------------
olfaction_plot <- 
  ggplot(olf_plot_df, aes(x = Genotype, y = Avoidance.Score)) + 
  geom_bar(stat="identity", fill = "cornflowerblue", width = 0.5) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  geom_errorbar(aes(ymin = Avoidance.Score - se, ymax = Avoidance.Score + se, 
                    width = 0.1)) + 
  theme_bw() + ylab("Mean Avoidance Score")

#ggsave(plot = olfaction_plot, filename = "Graphs/olfaction plot.png", 
 #      width = 6.25, height = 5)

# Significance test-------------------------------------------------------------
shapiro.test(olfaction_data_c) # 0.01979 not normal
shapiro.test(olfaction_data_m1) # 0.1084 normal
shapiro.test(olfaction_data_m2) # 0.2858 normal

# F test to see variances
var.test(olfaction_data_c, olfaction_data_m1, alternative = "two.sided") 
#   0.1848668 not significant difference in variance
var.test(olfaction_data_c, olfaction_data_m2, alternative = "two.sided") 
#   0.873463 not significant difference in variance

# Non-paired t-test
t.test(olfaction_data_c, olfaction_data_m1, alternative = "two.sided",
       mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
# 2.2e-16 significant

t.test(olfaction_data_c, olfaction_data_m2, alternative = "two.sided",
       mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
# 0.332 not significant