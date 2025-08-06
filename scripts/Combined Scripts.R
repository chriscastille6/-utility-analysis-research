set.seed(12345)

library(ggplot2)
library(gridExtra)

# build distributions
# Parameters
mean1 <- 0.42 # stuctured interview operational validity
sd1 <- 0.19
mean2 <- 0.19 # unstructured interview operational validity
sd2 <- 0.16

# Mean and standard deviation of the difference
mean_diff <- mean1 - mean2
sd_diff <- round(sqrt(sd1^2 + sd2^2),2)

# Create a sequence of x values
x <- seq(-1, 1, length.out = 1000)

# Create data frames for each distribution
df1 <- data.frame(x = x, y = dnorm(x, mean1, sd1), type = 'Structured Interviews')
df2 <- data.frame(x = x, y = dnorm(x, mean2, sd2), type = 'Unstructured Interviews')
df_diff <- data.frame(x = x, y = dnorm(x, mean_diff, sd_diff), type = 'Difference')

# Split each data frame based on x value
df1_neg <- df1[df1$x < 0, ]
df1_nonneg <- df1[df1$x >= 0, ]
df2_neg <- df2[df2$x < 0, ]
df2_nonneg <- df2[df2$x >= 0, ]
df_diff_neg <- df_diff[df_diff$x < 0, ]
df_diff_nonneg <- df_diff[df_diff$x >= 0, ]

# Combine data frames
df <- rbind(df1_neg, df1_nonneg, df2_neg, df2_nonneg, df_diff_neg, df_diff_nonneg)

# Create the plot
p <- ggplot() +
  geom_area(data = subset(df, x >= 0), aes(x = x, y = y, fill = 'red'), alpha = 0.4, show.legend = FALSE) +
  geom_area(data = subset(df, x < 0), aes(x = x, y = y, fill = 'green'), alpha = 0.4, show.legend = FALSE) +
  labs(title = 'Fig 1.', subtitle = 'Unstructured interviews outperform structured interviews ~18% of the time.', x = 'Operational Validity', y = 'Density', 
       caption = 'Note: Operational validity refers to an "estimate of the relationship between a predictor used in the practical context of selection and the theoretical construct that a criterion intends to measure" (Binning & Barrett, 1989).\nThe effect sizes are corrected correlations takent from Sackett et al. (2021). \nThe difference between the two distributions was computed by taking the difference between the mean operational validities. \nThe standard deviation for the difference is computed by squaring each individual standard deviation, summing them, and taking the square root of the total, \nreflecting the variability of the differences between these two validity measures.') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0), plot.caption = element_text(hjust = 0)) +
  facet_wrap(~factor(type, levels = c('Structured Interviews', 'Unstructured Interviews', 'Difference')), scales = 'free', ncol = 1)

# Add annotations for each facet
p <- p + geom_text(data = df1, aes(x = 0.7, y = 0.1, label = paste('Mean = ', mean1, '\nSD = ', sd1, '\n% Negative: ', round(pnorm(0, mean1, sd1) * 100, 2), '%')), hjust = 'right', inherit.aes = FALSE)
p <- p + geom_text(data = df2, aes(x = 0.7, y = 0.1, label = paste('Mean = ', mean2, '\nSD = ', sd2, '\n% Negative: ', round(pnorm(0, mean2, sd2) * 100, 2), '%')), hjust = 'right', inherit.aes = FALSE)
p <- p + geom_text(data = df_diff, aes(x = 0.7, y = 0.1, label = paste('Mean = ', mean_diff, '\nSD = ', sd_diff, '\n% Negative: ', round(pnorm(0, mean_diff, sd_diff) * 100, 2), '%')), hjust = 'right', inherit.aes = FALSE)

# Add annotations for each facet
p <- p + geom_text(data = df1, aes(x = 0.7, y = 0.1, label = paste('Mean = ', mean1, '\nSD = ', sd1, '\n% Negative: ', round(pnorm(0, mean1, sd1) * 100, 2), '%')), hjust = 'right', inherit.aes = FALSE)
p <- p + geom_text(data = df2, aes(x = 0.7, y = 0.1, label = paste('Mean = ', mean2, '\nSD = ', sd2, '\n% Negative: ', round(pnorm(0, mean2, sd2) * 100, 2), '%')), hjust = 'right', inherit.aes = FALSE)
p <- p + geom_text(data = df_diff, aes(x = 0.7, y = 0.1, label = paste('Mean = ', mean_diff, '\nSD = ', sd_diff, '\n% Negative: ', round(pnorm(0, mean_diff, sd_diff) * 100, 2), '%')), hjust = 'right', inherit.aes = FALSE)

# Add annotations for each facet
p <- p + geom_text(data = df1, aes(x = -0.3, y = 0.1, label = paste('% Negative: ', round(pnorm(0, mean1, sd1) * 100, 2), '%')), colour = 'black', inherit.aes = FALSE)
p <- p + geom_text(data = df2, aes(x = -0.3, y = 0.1, label = paste('% Negative: ', round(pnorm(0, mean2, sd2) * 100, 2), '%')), colour = 'black', inherit.aes = FALSE)
p <- p + geom_text(data = df_diff, aes(x = -0.3, y = 0.1, label = paste('% Negative: ', round(pnorm(0, mean_diff, sd_diff) * 100, 2), '%')), colour = 'black', inherit.aes = FALSE)

# Add mean lines for each facet
p <- p + geom_vline(data = df1, aes(xintercept = mean1), linetype = 'dashed', color = 'black')
p <- p + geom_vline(data = df2, aes(xintercept = mean2), linetype = 'dashed', color = 'black')
p <- p + geom_vline(data = df_diff, aes(xintercept = mean_diff), linetype = 'dashed', color = 'black')

# Print the plot
print(p)
