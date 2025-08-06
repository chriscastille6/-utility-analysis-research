library(ggplot2)

# Define the range of x values
x <- seq(-4, 4, by = 0.01)

# Define the normal distribution
y <- dnorm(x, mean = 0, sd = 1)

# create dataframe
df <- data.frame(x, y)

# Create the plot
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  geom_area(data = subset(df, x < 0), fill = 'red') +
  geom_area(data = subset(df, x > 0), fill = 'green') +
  theme_minimal()
