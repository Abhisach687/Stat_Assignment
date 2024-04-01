setwd("C:/Users/abhis/OneDrive/Desktop/datasets/")

# Loading required libraries
library(matlib)
library(visdat)
library(ggplot2)
library(glmnet)
library(rsample)
library(MASS)

# Reading CSV File
customer_shopping = read.csv("./customer_shopping_data_1695379411426.csv")

# Understanding dataset
head(customer_shopping)
tail(customer_shopping)
summary(customer_shopping)

# Check for missing values
missing_values <- is.na(customer_shopping)
missing_count <- colSums(missing_values)
print(missing_count)

# Exploring unique values
unique_genders <- unique(customer_shopping$gender)
print(unique_genders)

unique_category <- unique(customer_shopping$category)
print(unique_category)

unique_payment_method <- unique(customer_shopping$payment_method)
print(unique_payment_method)

unique_shopping_mall <- unique(customer_shopping$shopping_mall)
print(unique_shopping_mall)

# Converting categorical variables to numerical
customer_shopping$gender <- as.numeric(factor(customer_shopping$gender, levels = unique(customer_shopping$gender)))
customer_shopping$category <- as.numeric(factor(customer_shopping$category, levels = unique(customer_shopping$category)))
customer_shopping$payment_method <- as.numeric(factor(customer_shopping$payment_method, levels = unique(customer_shopping$payment_method)))
customer_shopping$shopping_mall <- as.numeric(factor(customer_shopping$shopping_mall, levels = unique(customer_shopping$shopping_mall)))

head(customer_shopping)

# Defining input features
x <- customer_shopping[, !(names(customer_shopping) %in% c("invoice_no", "customer_id", "quantity", "invoice_date", "gender", "shopping_mall"))]

# Converting invoice_date to Date format
customer_shopping$invoice_date <- as.Date(customer_shopping$invoice_date, format = "%d/%m/%Y")

# Creating time series object for input features
customer_shopping.ts <- ts(x,
                           start = c(as.numeric(format(min(customer_shopping$invoice_date), "%Y")),
                                     as.numeric(format(min(customer_shopping$invoice_date), "%m"))),
                           end = c(as.numeric(format(max(customer_shopping$invoice_date), "%Y")),
                                   as.numeric(format(max(customer_shopping$invoice_date), "%m"))),
                           frequency = 12)

# Plotting time series of input features
plot(customer_shopping.ts, main = "Time series plot of Input", xlab = "Invoice Date", ylab = "X (inputs)")

# Task 1.1 - Plotting time series of output (quantity)
customer_shopping$year_month <- format(customer_shopping$invoice_date, "%Y-%m")
aggregated_data <- aggregate(quantity ~ year_month, data = customer_shopping, sum)
aggregated_data$year_month <- as.Date(paste0(aggregated_data$year_month, "-01"))

customer_shopping.ts <- ts(aggregated_data$quantity,
                           start = c(as.numeric(format(min(aggregated_data$year_month), "%Y")),
                                     as.numeric(format(min(aggregated_data$year_month), "%m"))),
                           end = c(as.numeric(format(max(aggregated_data$year_month), "%Y")),
                                   as.numeric(format(max(aggregated_data$year_month), "%m"))),
                           frequency = 12)

plot(customer_shopping.ts, main = "Time Series Plot of Output (Grouped by Year-Month)", xlab = "Year-Month", ylab = "Total Quantity")

# Task 1.2 - Distribution of input and output variables
dis = density(x$price)
plot(dis, main = "Density plot of price")
hist(x$price, freq = FALSE, main = "Histogram and density plot of price", xlab = "Price")
lines(dis, lwd = 2, col = "black")
rug(jitter(x$price))

dis = density(x$payment_method)
plot(dis, main = "Density plot of payment_method")
hist(x$payment_method, freq = FALSE, main = "Histogram and density plot of payment Method", xlab = "Payment Method")
lines(dis, lwd = 2, col = "black")
rug(jitter(x$payment_method))

dis = density(x$age)
plot(dis, main = "Density plot of age")
hist(x$age, freq = FALSE, main = "Histogram and density plot of age")
lines(dis, lwd = 2, col = "black")
rug(jitter(x$age))

dis = density(x$category)
plot(dis, main = "Density plot of category")
hist(x$category, freq = FALSE, main = "Histogram and density plot of category")
lines(dis, lwd = 2, col = "black")
rug(jitter(x$category))

dis = density(customer_shopping$quantity)
plot(dis, main = "Density plot of quantity")
hist(customer_shopping$quantity, freq = FALSE, main = "Histogram and density plot of Quantity")
lines(dis, lwd = 2, col = "black")
rug(jitter(x$customer_shopping$quantity))

# Task 1.3 - Correlation and scatter plots
Y <- customer_shopping$quantity
plot(x$age, Y, main = "Correlation between age and quantity", xlab = "age", ylab = "quantity")
plot(x$price, Y, main = "Correlation between price and quantity", xlab = "price", ylab = "quantity")
plot(x$category, Y, main = "Correlation between category and quantity", xlab = "category", ylab = "quantity")
plot(x$payment_method, Y, main = "Correlation between payment_method and quantity", xlab = "payment_method", ylab = "quantity")

x$quantity <- customer_shopping$quantity
cor(x)
plot(x)

# Task 2 - Regression modeling
x$X1 <- x$age
x$X2 <- x$category
x$X3 <- x$price
x$X4 <- x$payment_method

x <- x[, c("X1", "X2", "X3", "X4")]
x <- as.matrix(x)
y <- as.matrix(customer_shopping$quantity)

ones <- matrix(1, length(x) / 4, 1)

# Task 2.1 - Fitting candidate models
alpha <- 0  # Ridge regression
lambda <- 1  # Adjust lambda value as needed

# Model 1
Y1 <- cbind(ones, (x[, "X4"]), (x[, "X1"])^2, (x[, "X1"])^3, (x[, "X2"])^4, (x[, "X1"])^4)
ridge_model1 <- glmnet(Y1, y, alpha = alpha, lambda = lambda)
thetaHatModel1 <- coefficients(ridge_model1)
print(thetaHatModel1)

# Model 2
Y2 <- cbind(ones, (x[, "X4"]), (x[, "X1"])^3, (x[, "X3"])^4)
ridge_model2 <- glmnet(Y2, y, alpha = alpha, lambda = lambda)
thetaHatModel2 <- coefficients(ridge_model2)
print(thetaHatModel2)

# Model 3
Y3 <- cbind(ones, (x[, "X3"])^3, (x[, "X3"])^4)
ridge_model3 <- glmnet(Y3, y, alpha = alpha, lambda = lambda)
thetaHatModel3 <- coefficients(ridge_model3)
print(thetaHatModel3)

# Model 4
Y4 <- cbind(ones, (x[, "X2"]), (x[, "X1"])^3, (x[, "X3"])^4)
ridge_model4 <- glmnet(Y4, y, alpha = alpha, lambda = lambda)
thetaHatModel4 <- coefficients(ridge_model4)
print(thetaHatModel4)

# Model 5
Y5 <- cbind(ones, (x[, "X4"]), (x[, "X1"])^2, (x[, "X1"])^3, (x[, "X3"])^4)
ridge_model5 <- glmnet(Y5, y, alpha = alpha, lambda = lambda)
thetaHatModel5 <- coefficients(ridge_model5)
print(thetaHatModel5)

# Task 2.2 - Calculating RSS for each model
Y_hat_ridge1 <- predict(ridge_model1, s = lambda, newx = Y1)
residuals_ridge <- y - Y_hat_ridge1
RSS_ridge <- sum(residuals_ridge^2)
coefficients_ridge <- coef(ridge_model1, s = lambda)
Y_hat_m1 <- as.matrix(Y1) %*% coefficients_ridge[-1]  # Excluding intercept
residuals_m1 <- y - Y_hat_m1
RSS_Model_1 <- sum(residuals_m1^2)
print(RSS_Model_1)

Y_hat_ridge2 <- predict(ridge_model2, s = lambda, newx = Y2)
residuals_ridge <- y - Y_hat_ridge2
RSS_ridge <- sum(residuals_ridge^2)
coefficients_ridge <- coef(ridge_model2, s = lambda)
Y_hat_m2 <- as.matrix(Y2) %*% coefficients_ridge[-1]
residuals_m2 <- y - Y_hat_m2
RSS_Model_2 <- sum(residuals_m2^2)
print(RSS_Model_2)

Y_hat_ridge3 <- predict(ridge_model3, s = lambda, newx = Y3)
residuals_ridge <- y - Y_hat_ridge3
RSS_ridge <- sum(residuals_ridge^2)
coefficients_ridge <- coef(ridge_model3, s = lambda)
Y_hat_m3 <- as.matrix(Y3) %*% coefficients_ridge[-1]
residuals_m3 <- y - Y_hat_m3
RSS_Model_3 <- sum(residuals_m3^2)
print(RSS_Model_3)

Y_hat_ridge4 <- predict(ridge_model4, s = lambda, newx = Y4)
residuals_ridge <- y - Y_hat_ridge4
RSS_ridge <- sum(residuals_ridge^2)
coefficients_ridge <- coef(ridge_model4, s = lambda)
Y_hat_m4 <- as.matrix(Y4) %*% coefficients_ridge[-1]
residuals_m4 <- y - Y_hat_m4
RSS_Model_4 <- sum(residuals_m4^2)
print(RSS_Model_4)

Y_hat_ridge5 <- predict(ridge_model5, s = lambda, newx = Y5)
residuals_ridge <- y - Y_hat_ridge5
RSS_ridge <- sum(residuals_ridge^2)
coefficients_ridge <- coef(ridge_model5, s = lambda)
Y_hat_m5 <- as.matrix(Y5) %*% coefficients_ridge[-1]
residuals_m5 <- y - Y_hat_m5
RSS_Model_5 <- sum(residuals_m5^2)
print(RSS_Model_5)

# Task 2.3 - Calculating log-likelihood for each model
N <- length(y)

# Model 1
Variance_model1 <- RSS_Model_1 / (N - 1)
likehood_Model_1 <- -(N / 2) * (log(2 * pi)) - (N / 2) * (log(Variance_model1)) - (1 / (2 * Variance_model1)) * RSS_Model_1
print(likehood_Model_1)

# Model 2
Variance_model2 <- RSS_Model_2 / (N - 1)
likehood_Model_2 <- -(N / 2) * (log(2 * pi)) - (N / 2) * (log(Variance_model2)) - (1 / (2 * Variance_model2)) * RSS_Model_2
print(likehood_Model_2)

# Model 3
Variance_model3 <- RSS_Model_3 / (N - 1)
likehood_Model_3 <- -(N / 2) * (log(2 * pi)) - (N / 2) * (log(Variance_model3)) - (1 / (2 * Variance_model3)) * RSS_Model_3
print(likehood_Model_3)

# Model 4
Variance_model4 <- RSS_Model_4 / (N - 1)
likehood_Model_4 <- -(N / 2) * (log(2 * pi)) - (N / 2) * (log(Variance_model4)) - (1 / (2 * Variance_model4)) * RSS_Model_4
print(likehood_Model_4)

# Model 5
Variance_model5 <- RSS_Model_5 / (N - 1)
likehood_Model_5 <- -(N / 2) * (log(2 * pi)) - (N / 2) * (log(Variance_model5)) - (1 / (2 * Variance_model5)) * RSS_Model_5
print(likehood_Model_5)

# Task 2.4 - Evaluating AIC and BIC for each model
# Model 1
K_model1 <- length(thetaHatModel1)
AIC_model1 <- 2 * K_model1 - 2 * likehood_Model_1
BIC_model1 <- K_model1 * log(N) - 2 * likehood_Model_1
print(paste("AIC for Model 1:", AIC_model1))
print(paste("BIC for Model 1:", BIC_model1))

# Model 2
K_model2 <- length(thetaHatModel2)
AIC_model2 <- 2 * K_model2 - 2 * likehood_Model_2
BIC_model2 <- K_model2 * log(N) - 2 * likehood_Model_2
print(paste("AIC for Model 2:", AIC_model2))
print(paste("BIC for Model 2:", BIC_model2))

# Model 3
K_model3 <- length(thetaHatModel3)
AIC_model3 <- 2 * K_model3 - 2 * likehood_Model_3
BIC_model3 <- K_model3 * log(N) - 2 * likehood_Model_3
print(paste("AIC for Model 3:", AIC_model3))
print(paste("BIC for Model 3:", BIC_model3))

# Model 4
K_model4 <- length(thetaHatModel4)
AIC_model4 <- 2 * K_model4 - 2 * likehood_Model_4
BIC_model4 <- K_model4 * log(N) - 2 * likehood_Model_4
print(paste("AIC for Model 4:", AIC_model4))
print(paste("BIC for Model 4:", BIC_model4))

# Model 5
K_model5 <- length(thetaHatModel5)
AIC_model5 <- 2 * K_model5 - 2 * likehood_Model_5
BIC_model5 <- K_model5 * log(N) - 2 * likehood_Model_5
print(paste("AIC for Model 5:", AIC_model5))
print(paste("BIC for Model 5:", BIC_model5))

# Task 2.5 - Checking distribution of model prediction errors (residuals)
model1_error <- y - Y_hat_m1
qqnorm(model1_error, col = "darkcyan", main = "QQ plot of model 1")
qqline(model1_error, col = "red", lwd = 1)

model2_error <- y - Y_hat_m2
qqnorm(model2_error, col = "darkcyan", main = "QQ plot of model 2")
qqline(model2_error, col = "red", lwd = 1)

model3_error <- y - Y_hat_m3
qqnorm(model3_error, col = "darkcyan", main = "QQ plot of model 3")
qqline(model3_error, col = "red", lwd = 1)

model4_error <- y - Y_hat_m4
qqnorm(model4_error, col = "darkcyan", main = "QQ plot of model 4")
qqline(model4_error, col = "red", lwd = 1)

model5_error <- y - Y_hat_m5
qqnorm(model5_error, col = "darkcyan", main = "QQ plot of model 5")
qqline(model5_error, col = "red", lwd = 1)

# Task 2.6 - Selecting the 'best' regression model
# Based on the AIC, BIC, and distribution of residuals, Model 3 appears to be the best choice.
# Model 3 has the lowest AIC and BIC values, and its residuals seem to follow a normal distribution.

# Task 2.7 - Splitting data into training and testing sets, estimating model parameters, and computing predictions
set.seed(123)  # Set seed for reproducibility

split_X <- initial_split(data = as.data.frame(x), prop = 0.7)
split_Y <- initial_split(data = as.data.frame(y), prop = 0.7)

X_training_set <- training(split_X)
X_testing_set <- testing(split_X)
Y_training_set <- as.matrix(training(split_Y))
Y_testing_set <- as.matrix(testing(split_Y))

# Create the design matrix for the selected 'best' model (Model 3)
training_ones <- matrix(1, nrow = nrow(X_training_set), ncol = 1)
X_training_model <- cbind(training_ones, (X_training_set[, "X3"])^3, (X_training_set[, "X3"])^4)

# Estimate model parameters using the training data
theta_hat <- ginv(t(X_training_model) %*% X_training_model) %*% t(X_training_model) %*% Y_training_set

# Create the design matrix for the testing data
testing_ones <- matrix(1, nrow = nrow(X_testing_set), ncol = 1)
X_testing_model <- cbind(testing_ones, (X_testing_set[, "X3"])^3, (X_testing_set[, "X3"])^4)

# Calculate model predictions on the testing data
Y_testing_hat <- X_testing_model %*% theta_hat

# Evaluating 95% confidence intervals for the model predictions
z <- qnorm(0.975)  # Z-score for 95% confidence interval
n_len <- nrow(X_testing_model)
error <- Y_testing_set - Y_testing_hat
valid_indices <- (error != 0)  # Check for non-zero error values

# Ensure that the values inside sqrt are non-negative using abs function
C_I_1 <- ifelse(valid_indices, z * sqrt(abs(error * (1 - error)) / n_len), 0)
C_I_2 <- ifelse(valid_indices, z * sqrt(abs(error * (1 + error)) / n_len), 0)

# Plotting
plot(Y_testing_set, col = "red", pch = 19, xlab = "Index", ylab = "Y Value", main = "Model Predictions and 95% Confidence Intervals")
points(Y_testing_hat, col = "blue", pch = 19)  # Add error bars for 95% confidence intervals
arrows(x0 = 1:n_len, y0 = Y_testing_hat - C_I_1, y1 = Y_testing_hat + C_I_2, angle = 90, code = 3, length = 0.1, col = "green")

# Legend
legend("topright", legend = c("Testing Data", "Model Predictions", "95% CI"), col = c("red", "blue", "green"), pch = 19, cex = 0.8)


# Task 3: Approximate Bayesian Computation (ABC)
# Using 'rejection ABC' method to compute the posterior distributions of the selected regression model parameters

# We'll compute the posterior distributions for the two parameters with the largest absolute values
# from the least squares estimation (Task 2.1) of the selected model (Model 3).

# Fixing all other parameters as constants using the estimated values from Task 2.1
theta_bias <- thetaHatModel3[1]  # Intercept term
theta_one <- thetaHatModel3[2]
theta_two <- thetaHatModel3[3]

# Specifying the prior distributions (Uniform) for the two parameters
prior_range_1 <- 0.1  # Range around the estimated value for the first parameter
prior_range_2 <- 0.1  # Range around the estimated value for the second parameter

num_iterations <- 100000  # Number of iterations for rejection ABC
accepted_values_1 <- numeric(num_iterations)  # Stores accepted values for parameter 1
accepted_values_2 <- numeric(num_iterations)  # Stores accepted values for parameter 2
rss_values <- numeric(num_iterations)  # Stores RSS values for each iteration

counter <- 0  # Counter for accepted values

# Performing rejection ABC
for (i in 1:num_iterations) {
  range1 <- runif(1, theta_one - prior_range_1, theta_one + prior_range_1)
  range2 <- runif(1, theta_two - prior_range_2, theta_two + prior_range_2)
  new_theta_hat <- c(theta_bias, range1, range2)
  new_Y_Hat <- Y3 %*% new_theta_hat
  new_RSS <- sum((y - new_Y_Hat)^2)
  rss_values[i] <- new_RSS
}

# Accept the top 5% samples with the lowest RSS values
acceptance_threshold <- quantile(rss_values, 0.05)
accepted_indices <- which(rss_values < acceptance_threshold)

accepted_values_1 <- as.numeric(unlist(lapply(accepted_indices, function(idx) {
  range1 <- runif(1, theta_one - prior_range_1, theta_one + prior_range_1)
  return(range1)
})))

accepted_values_2 <- as.numeric(unlist(lapply(accepted_indices, function(idx) {
  range2 <- runif(1, theta_two - prior_range_2, theta_two + prior_range_2)
  return(range2)
})))

# Plotting the histograms of accepted values (marginal posterior distributions)
hist(accepted_values_1, main = "Histogram of Accepted Values (Parameter 1)", xlab = "Parameter 1")
hist(accepted_values_2, main = "Histogram of Accepted Values (Parameter 2)", xlab = "Parameter 2")

# Plotting the joint posterior distribution
plot(accepted_values_1, accepted_values_2, col = "green", main = "Joint Posterior Distribution", xlab = "Parameter 1", ylab = "Parameter 2")

# Explanation of results
cat("The histograms represent the marginal posterior distributions of the two parameters obtained using the rejection ABC method.",
    "The joint posterior distribution plot shows the correlation between the two parameters.",
    "These posterior distributions provide an approximation of the parameter values that best fit the observed data under the chosen model.",
    "The results can be used to quantify the uncertainty in the parameter estimates and make predictions with associated confidence intervals.",
    sep = "\n")