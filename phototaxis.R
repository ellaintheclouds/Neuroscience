library(ggplot2)

phototaxis_data <- read.csv("Data/phototaxis.csv")
phototaxis_data <- phototaxis_data[1:116, 1:7]
colnames(phototaxis_data) <- c("genotype", "1min", "2min", "3min", "4min", 
                               "5min", "phototaxis_score")

split <- split(phototaxis_data, phototaxis_data$genotype)

phototaxis_data_c <- split$C
phototaxis_data_c <- phototaxis_data_c[, 2:7]
mean_c <- apply(phototaxis_data_c, 2, mean) # control mean
sd_c <- apply(phototaxis_data_c, 2, sd) # control sd
se_c <- (sd_c)/sqrt(38) # se


phototaxis_data_m1 <- split$M1
phototaxis_data_m1 <- phototaxis_data_m1[, 2:7]
mean_m1 <- apply(phototaxis_data_m1, 2, mean) #  mean
sd_m1 <- apply(phototaxis_data_m1, 2, sd) #  sd
se_m1 <- (sd_m1)/sqrt(39) # se

phototaxis_data_m2 <- split$M2
phototaxis_data_m2 <- phototaxis_data_m2[, 2:7]
mean_m2 <- apply(phototaxis_data_m2, 2, mean) #  mean
sd_m2 <- apply(phototaxis_data_m2, 2, sd) #  sd
se_m2 <- (sd_m2)/sqrt(39) # se

plot_df <- data.frame("time" = c(1, 2, 3, 4, 5), 
                      "mean_c" = mean_c[1:5],
                      "se_c" = se_c[1.5], 
                      "mean_m1" = mean_m1[1:5], 
                      "se_m1" = se_m1[1:5], 
                      "mean_m2" = mean_m2[1:5], 
                      "se_m2" = se_m2[1:5])

phototaxis_plot <- 
  ggplot(plot_df, aes(x = time)) + 
  geom_point(aes(y = mean_c), colour = "green4") +
  geom_point(aes(y = mean_m1), colour = "cornflowerblue") +
  geom_point(aes(y = mean_m2), colour = "red4") + 
  
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c, width = 0.1),
                colour = "green4") + 
  geom_errorbar(aes(ymin = mean_m1 - se_m1, ymax = mean_m1 + se_m1, width = 0.1),
                colour = "cornflowerblue") + 
  geom_errorbar(aes(ymin = mean_m2 - se_m2, ymax = mean_m2 + se_m2, width = 0.1),
                colour = "red4") + 
  
  geom_smooth(aes(x = time, y = mean_c), method = 'lm', 
              colour = "green4", se = FALSE) + 
  geom_smooth(aes(x = time, y = mean_m1), method = 'lm', 
              colour = "cornflowerblue", se = FALSE) + 
  geom_smooth(aes(x = time, y = mean_m2), method = 'lm', 
              colour = "red4", se = FALSE) + 
  
  theme_bw() + 
  xlab("Time (Minutes)") + ylab("Mean Number of Flies in Light")

#ggsave(plot = phototaxis_plot, filename = "Graphs/phototaxis plot.png", 
 #      width = 6.25, height = 5)
