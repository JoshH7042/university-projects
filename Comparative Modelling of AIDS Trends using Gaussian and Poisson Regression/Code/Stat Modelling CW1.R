## Statistical Modelling Coursework

load("C:/Users/joshh/OneDrive - University of Exeter/MSc Applied Data Science and Statistics/MTHM506 - Statistical Data Modelling/Coursework 1/datasets.RData")
library(ggplot2)
library(rmarkdown)
library(tidyverse)
library(lmerTest)

library(ggplot2)

ggplot(nlmodel, aes(x = x, y = y)) +
  geom_point(color = "black", size = 1.5, alpha = 0.6) +  
  labs(title = "Scatter Plot of Y vs X", x = "X", y = "Y") +
  theme_classic(base_size = 12) +  
  theme(
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),  
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 10)
  )


# Define the negative log-likelihood function 

mylike <- function(params, y, x) {
  theta1 <- params[1]
  theta2 <- params[2]
  sigma <- abs(params[3])
  
  # Number of Observations
  
  n <- length(y)
  
  # Mean function
  
  mu <- (theta1 * x) / (theta2 + x)
  
  # Computing log-likelihood
  
  logL <- - (n /2) * log(2 * pi * sigma^2) - sum((y - mu)^2) / (2 * sigma^2)
  
  # Return negative log-likelihood
  
  return(-logL)
  
}


x <- nlmodel$x
y <- nlmodel$y

# 1. Choosing starting values:

# For Theta 1: I have chosen to use the range of y divided by the range of x

theta1_start <- (max(y) - min(y)) / (max(x) - min(x))

# For Theta 2: I have chosen to use the median of x in order to avoid extreme values in the denominator

theta2_start <- median(x)

# For Sigma: I have chosen to use the standard deviation of y as an estimate

sigma_start <- sd(y)

#Combining into a vector 

start_values <- c(theta1_start, theta2_start, sigma_start)

# 2. Minimising the negative log-likelihood using nlm()

fit <- nlm(mylike, p = start_values, y = y, x = x, hessian=TRUE)

# 3. Extract MLE estimates

theta1_hat <- fit$estimate[1]
theta2_hat <- fit$estimate[2]
sigma_hat <- abs(fit$estimate[3]) #ensuring sigma is positive

#Printing result (maybe hide this part in the report and just show output)

cat("Maximum Likelihood Estimates (MLE's):\n")
cat("Theta1:", theta1_hat, "\n")
cat("Theta2:", theta2_hat, "\n")
cat("Sigma:", sigma_hat, "\n")

# 4. Plotting data and fitted mean function

fitted_mu <- (theta1_hat * x) / (theta2_hat + x)

ggplot() +
  geom_point(aes(x, y), color = "black") +  # Scatter plot of data
  geom_line(aes(x, fitted_mu), color = "red", size = 1) +  # Fitted mean function
  labs(title = "Scatter Plot of y vs x with Fitted Mean Function",
       x = "x", y = "y") +
  theme_minimal()

#Computing Hessian and variance-covariance matrix

result$hessian 



#Computing at a 95% confidence interval 

ci_theta1 <- c(theta1_hat - 1.96 * se_theta1, theta1_hat + 1.96 * se_theta1)
ci_theta2 <- c(theta2_hat - 1.96 * se_theta2, theta2_hat + 1.96 * se_theta2)

# Creating a data frame for displaying results
results_df <- data.frame(
  Parameter = c("Theta1", "Theta2"),
  `Std. Error` = c(sprintf("%.3f", se_theta1), sprintf("%.6f", se_theta2)),
  `95% CI Lower` = c(sprintf("%.3f", ci_theta1[1]), sprintf("%.6f", ci_theta2[1])),
  `95% CI Upper` = c(sprintf("%.3f", ci_theta1[2]), sprintf("%.6f", ci_theta2[2]))
)

# Display results in a nice table format
knitr::kable(results_df, caption = "Standard Errors and 95% Confidence Intervals")

theta2_0 <- 0.08 #H0: theta2 = 0.08

# Computing z-test statistic 

Z_score <- (theta2_hat - theta2_0) / se_theta2
Z_score

# Computing two-tailed p-value

p_value <- 2 * (1 - pnorm(abs(Z_score))) 
p_value

predicted_y <- (theta1_hat * x) / (theta2_hat + x)

# Compute 95% prediction interval bounds

lower_bound <- predicted_y - 1.96 * sigma_hat
upper_bound <- predicted_y + 1.96 * sigma_hat

# dataframe for plotting

prediction_df <- data.frame(
  
  x = x, 
  y = y, 
  predicted_y = predicted_y,
  lower = lower_bound,
  upper = upper_bound
)

# Plot data, predicted values, and confidence interval 
library(ggplot2)

ggplot(prediction_df, aes(x, y)) +
  geom_point(color = "blue", size = 1.5, alpha = 0.6) +
  geom_line(aes(y = predicted_y), color = "red", linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray", alpha = 0.3) +
  labs(title = "Plug-in Prediction with 95% Prediction Interval",
       x = 'x', y = "y") +
  theme_classic(base_size = 12) +  
  theme(
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),  
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 10)
)


# Visualising yi against xi in the aids dataframe. 

library(zoo)

aids$date <- as.Date(as.yearqtr(aids$date, format="%YQ%q"))

year_corrected <- year(aids$date) + 1900

aids$date <- make_date(year_corrected, month(aids$date), day(aids$date))

str(aids)

mean_cases <- mean(aids$cases, na.rm = TRUE)

ggplot(aids, aes(x = date, y = cases)) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +  
  labs(title = "Quarterly AIDS Cases in the UK",
       x = "Year",
       y = "Number of Cases") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  # Fix x-axis labels
  theme_classic(base_size = 12) +  
  theme(
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),  
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 10)
  )


hist(aids$cases, breaks = 10, main = "Distribution of AIDS Cases", xlab = "Cases")

mean_cases <- mean(aids$cases)
var_cases <- var(aids$cases)
mean_cases
var_cases

hist(log(aids$cases), breaks = 10, main = "Distribution of log(AIDS Cases)", xlab = "log(Cases)")

ggplot(aids, aes(x = date, y = log(cases))) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Log(Cases) over Time",
       x = "Time (Year)",
       y = "log(Cases)") +
  theme_minimal()
