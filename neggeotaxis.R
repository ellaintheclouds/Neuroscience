# Set up------------------------------------------------------------------------
library(ggplot2)

geotaxis_data <- read.csv("Data/neggeotaxis.csv")


# Data processing---------------------------------------------------------------
geotaxis_data <- geotaxis_data[1:110, 1:2]

split_genotype <- split(geotaxis_data, geotaxis_data$Genotype)

c_data <- split_genotype$C
c_mean <- mean(c_data$Performance.Index)
c_sd <- sd(c_data$Performance.Index)
c_se <- (c_sd)/sqrt(108)

m1_data <- split_genotype$M1
m1_mean <- mean(m1_data$Performance.Index)
m1_sd <- sd(m1_data$Performance.Index)
m1_se <- (m1_sd)/sqrt(109)

m2_data <- split_genotype$M2
m2_mean <- mean(m2_data$Performance.Index)
m2_sd <- sd(m2_data$Performance.Index)
m2_se <- (m2_sd)/sqrt(110)

# Create a data frame for plotting
plot_df <- data.frame("genotype" = c("c", "m1", "m2"),
                      "mean" = c(c_mean, m1_mean, m2_mean),
                      "se" = c(c_se, m1_se, m2_se))


# Plot--------------------------------------------------------------------------
geotaxis_plot <- 
  ggplot(data = plot_df, aes(x = genotype, y = mean)) + 
  geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") +
  geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_x_discrete(labels=c("Control", "Mutant 1", "Mutant 2")) + 
  xlab("Genotype") + ylab("Mean Performance Index") + 
  theme_bw()
  
#ggsave(plot = geotaxis_plot, filename = "Graphs/negative geotaxis plot.png", 
 #      width = 6.25, height = 5)


# Significance testing----------------------------------------------------------
var.test(c_data$Performance.Index, m1_data$Performance.Index, 
         alternative = "two.sided") 
# 0.1569 equal variance

var.test(c_data$Performance.Index, m2_data$Performance.Index, 
         alternative = "two.sided") 
# 0.8975 equal variance

t.test(c_data$Performance.Index, m1_data$Performance.Index,
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE, 
       conf.level = 0.95)
# 0.7789 not significant

t.test(c_data$Performance.Index, m2_data$Performance.Index,
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE, 
       conf.level = 0.95)
# 0.9444 not significant
