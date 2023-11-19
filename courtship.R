library(ggplot2)

data <- read.csv("Data/courtship.csv")
data <- data[,1:6]

split_data <- split(data, data$Genotype)

# Control
c_data <- split_data$Control

c_l_mean <- mean(c_data$Courtship.initiation.latency..seconds.)
c_l_sd <- sd(c_data$Courtship.initiation.latency..seconds.)
c_l_se <- (c_l_sd)/sqrt(26)

c_ci_mean <- mean(c_data$Courtship.Index..CI.)
c_ci_sd <- sd(c_data$Courtship.Index..CI.)
c_ci_se <- (c_ci_sd)/sqrt(26)

# Mutant 1
m1_data <- split_data$`Mutant 1`

m1_l_mean <- mean(m1_data$Courtship.initiation.latency..seconds.)
m1_l_sd <- sd(m1_data$Courtship.initiation.latency..seconds.)
m1_l_se <- (m1_l_sd)/sqrt(26)

m1_ci_mean <- mean(m1_data$Courtship.Index..CI.)
m1_ci_sd <- sd(m1_data$Courtship.Index..CI.)
m1_ci_se <- (m1_ci_sd)/sqrt(26)

# Mutant 2
m2_data <- split_data$`Mutant 2`

m2_l_mean <- mean(m2_data$Courtship.initiation.latency..seconds.)
m2_l_sd <- sd(m2_data$Courtship.initiation.latency..seconds.)
m2_l_se <- (m2_l_sd)/sqrt(26)

m2_ci_mean <- mean(m2_data$Courtship.Index..CI.)
m2_ci_sd <- sd(m2_data$Courtship.Index..CI.)
m2_ci_se <- (m2_ci_sd)/sqrt(26)

plot_df <- data.frame("genotype" = c("c", "m1", "m2"),
                      "mean_l" = c(c_l_mean, m1_l_mean, m2_l_mean), 
                      "se_l" = c(c_l_se, m1_l_se, m2_l_se),
                      "mean_ci" = c(c_ci_mean, m1_ci_mean, m2_ci_mean),
                      "se_ci" = c(c_ci_se, m1_ci_se, m2_ci_se))
# Plot--------------------------------------------------------------------------
latency_plot <- 
  ggplot(plot_df, aes(x = genotype, y = mean_l)) +
  geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") +
  geom_errorbar(aes(ymin = mean_l-se_l, ymax = mean_l + se_l, width = 0.1)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("Control", "Mutant 1", "Mutant 2")) + 
  xlab("Genotype") + ylab("Mean Initiation Latency (Seconds)") + 
  theme_bw()
  
index_plot <- 
  ggplot(plot_df, aes(x = genotype, y = mean_ci)) +
  geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") +
  geom_errorbar(aes(ymin = mean_ci-se_ci, ymax = mean_ci + se_ci, width = 0.1)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("Control", "Mutant 1", "Mutant 2")) + 
  xlab("Genotype") + ylab("Mean Courtship Index") + 
  theme_bw()

#ggsave(plot = latency_plot, filename = "Graphs/courtship latency plot.png", 
 #      width = 6.25, height = 5)

#ggsave(plot = index_plot, filename = "Graphs/courtship index plot.png", 
 #      width = 6.25, height = 5)

# Significance------------------------------------------------------------------
# Test if normally distributed
# I have chosen to use the Shapiro-Wilk's test (recommended for small samples)
# A p-value >0.05 implies normal data
shapiro.test(c_data$Courtship.initiation.latency..seconds.) # normal
shapiro.test(m1_data$Courtship.initiation.latency..seconds.) # not normal
shapiro.test(m2_data$Courtship.initiation.latency..seconds.) # not normal

shapiro.test(c_data$Courtship.Index..CI.) # not normal
shapiro.test(m1_data$Courtship.Index..CI.) # normal
shapiro.test(m2_data$Courtship.Index..CI.) # not normal

# Since the majority are not normal, let's do non-parametric analysis
# Paired Wilcoxon test
wilcox_c_m1 <- wilcox.test(c_data$Courtship.initiation.latency..seconds., 
            m1_data$Courtship.initiation.latency..seconds., 
            alternative = "two.sided")
wilcox_c_m1$p.value # 0.001356899 significant diff

wilcox_c_m2 <- wilcox.test(c_data$Courtship.initiation.latency..seconds., 
                           m2_data$Courtship.initiation.latency..seconds., 
                           alternative = "two.sided")
wilcox_c_m2$p.value # 0.06303843


ci_wilcox_c_m1 <- wilcox.test(c_data$Courtship.Index..CI., 
                           m1_data$Courtship.Index..CI., 
                           alternative = "two.sided")
ci_wilcox_c_m1$p.value # 4.775778e-05 significant diff

ci_wilcox_c_m2 <- wilcox.test(c_data$Courtship.Index..CI., 
                              m2_data$Courtship.Index..CI., 
                              alternative = "two.sided")
ci_wilcox_c_m2$p.value #  0.001076902 significant diff