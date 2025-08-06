# load packages
library(ggplot2)
library(gridExtra)

# build distributions
# Parameters
# stuctured interview operational validity
mean1 <- 0.42
sd1 <- 0.19

# unstructured interview operational validity
mean2 <- 0.19
sd2 <- 0.16

# Mean and standard deviation of the difference
mean_diff <- mean1 - mean2
sd_diff <- sqrt(sd1^2 + sd2^2)

# Create a sequence of x values
x <- seq(-1, 1, length.out = 1000)

# Create data frames for each distribution
df1 <- data.frame(x = x, y = dnorm(x, mean1, sd1), type = "Structured Interviews")
df2 <- data.frame(x = x, y = dnorm(x, mean2, sd2), type = "Unstructured Interviews")
df_diff <- data.frame(x = x, y = dnorm(x, mean_diff, sd_diff), type = "Difference")
df_diff_neg <- df_diff[df_diff$x < 0, ]
df_diff_nonneg <- df_diff[df_diff$x >= 0, ]

# Combine data frames
df <- rbind(df1, df2, df_diff_nonneg)
df_neg <- df_diff_neg

# Calculate the proportion that is negative for each distribution
prop_neg1 <- pnorm(0, mean1, sd1)
prop_neg2 <- pnorm(0, mean2, sd2)
prop_neg_diff <- pnorm(0, mean_diff, sd_diff)

# Create the plot
p <- ggplot() +
  geom_area(data = df, aes(x = x, y = y, fill = type), alpha = 0.4) +
  geom_area(data = df_neg, aes(x = x, y = y, fill = type), alpha = 0.4) +
  scale_fill_manual(values = c("Structured Interviews" = "blue", "Unstructured Interviews" = "red", "Difference" = "gray")) +
  labs(x = "Operational Validity", y = "Density") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  facet_wrap(~factor(type, levels = c("Structured Interviews", "Unstructured Interviews", "Difference")), scales = "free", ncol = 1)

# Add annotations for each facet
p <- p + geom_text(data = df1, aes(x = -0.3, y = 0.1, label = paste("Percentage Negative: ", round(prop_neg1 * 100, 2), "%")), inherit.aes = FALSE)
p <- p + geom_text(data = df2, aes(x = -0.3, y = 0.1, label = paste("Percentage Negative: ", round(prop_neg2 * 100, 2), "%")), inherit.aes = FALSE)
p <- p + geom_text(data = df_diff, aes(x = -0.3, y = 0.1, label = paste("Percentage Negative: ", round(prop_neg_diff * 100, 2), "%")), inherit.aes = FALSE)

# Print the plot
print(p)
