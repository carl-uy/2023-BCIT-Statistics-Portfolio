# Load the salary data from the URL
library(data.table)
salary_data <- fread("http://mathweb.bcit.ca/data/salary.csv")

# Assign the year and salary vectors
y <- salary_data$year
s <- salary_data$salary

# Plot Salary against Year
plot(y, s, main = "Mean Annual Wage in BC (1926-1975)", xlab = "Year", ylab = "Salary")

# Plot log(Salary) against Year to check for linear correlation
plot(y, log(s), main = "Logarithm of Mean Annual Wage in BC (1926-1975)", xlab = "Year", ylab = "Log(Salary)")
# Take the natural logarithm of s
ls <- log(s)

# Compute the linear correlation coefficient r using the formula in Worksheet #10
n <- length(y)
numerator <- n * sum(y * ls) - sum(y) * sum(ls)
denominator <- sqrt((n * sum(y^2) - sum(y)^2) * (n * sum(ls^2) - sum(ls)^2))
r <- numerator / denominator
print(paste0("Linear correlation coefficient r: ", round(r, 4)))

# Compute the slope and intercept of the regression line
b1 <- r * (sd(ls) / sd(y))
b0 <- mean(ls) - b1 * mean(y)
print(paste0("Regression equation: ls = ", round(b1, 4), "*y + ", round(b0, 4)))

# Plot the regression line along with the data points
reg_line <- b1 * y + b0
plot(y, ls, main = "Logarithm of Mean Annual Wage in BC (1926-1975)", xlab = "Year", ylab = "Log(Salary)")
points(y, reg_line, col = "red", type = "l")

# Compute the 95% confidence interval for b1
t_star <- qt(0.975, n - 2)
SE <- sqrt(sum((ls - reg_line)^2) / (n - 2)) / sqrt(sum((y - mean(y))^2))
lower_bound <- b1 - t_star * SE
upper_bound <- b1 + t_star * SE
print(paste0("95% Confidence interval for b1: (", round(lower_bound, 4), ", ", round(upper_bound, 4), ")"))

# Extrapolate the data to the present (y = 2017) using the regression equation
extrapolated_ls <- b1 * 2017 + b0
extrapolated_salary <- exp(extrapolated_ls)
print(paste0("Extrapolated mean annual wage in BC for 2017: $", round(extrapolated_salary, 2)))