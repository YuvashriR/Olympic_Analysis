# Load the dataset
data <- read.csv("C:/Users/Admin/OneDrive - University of Leeds/Desktop/Statistical Learning/medal_pop_gdp_data_statlearn.csv")

# Load required libraries
library(tidyverse)
library(MASS) # For log transformation
library(leaps) # For AIC-based model selection


# Exploratory Data Analysis (EDA)
# Display structure of the data
print("Data structure:")
print(str(data))

# Set up a 1 x 3 layout
par(mfrow = c(1, 3))

# Create box and whiskers plot for GDP
boxplot(data$GDP, main = "Boxplot of GDP", ylab = "GDP")

# Create box and whiskers plot for Population
boxplot(data$Population, main = "Boxplot of Population", ylab = "Population")

# Create box and whiskers plot for Medals2012
boxplot(data$Medal2012, main = "Boxplot of Medals2012", ylab = "Medals2012")


# Summary statistics
print("Summary statistics:")
print(summary(data))

# Print correlation matrix
print("Correlation matrix:")
cor_matrix <- cor(data[,c("GDP", "Population", "Medal2012")])
print(cor_matrix)

# Plot correlation matrix with values
print("Plotting correlation matrix:")
corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Set up a 1 x 2 layout
par(mfrow = c(1, 2))

# Scatter plot for GDP vs. Medal count
cor_coefficient_gdp <- round(cor(data$GDP, data$Medal2012), 2)
plot(data$GDP, data$Medal2012, 
     main = "GDP vs. Medal count", 
     xlab = "GDP (in billions of US dollars)", 
     ylab = "Medal count", 
     col = "blue")
abline(lm(Medal2012 ~ GDP, data = data), col = "red")
legend("topleft", legend = paste("Correlation =", cor_coefficient_gdp), bty = "n")

# Scatter plot for Population vs. Medal count
cor_coefficient_population <- round(cor(data$Population, data$Medal2012), 2)
plot(data$Population, data$Medal2012, 
     main = "Population vs. Medal count", 
     xlab = "Population", 
     ylab = "Medal count", 
     col = "blue")
abline(lm(Medal2012 ~ Population, data = data), col = "red")
legend("topleft", legend = paste("Correlation =", cor_coefficient_population), bty = "n")




# Task 1: Linear regression model with Population and GDP as inputs and Medal count in 2012 Olympics as output
print("Task 1: Linear regression model with Population and GDP as inputs")
lm_model <- lm(Medal2012 ~ Population + GDP, data = data)
print("Model Summary:")
print(summary(lm_model))

# Plot regression line
print("Linear regression plot:")
print(plot(lm_model))


# Task 2: Repeat task 1 for log-transformed outputs
# Load the required library
######Log transformation
data$log_Medal2012 <- log(data$Medal2012 + 1)

# Set up a multi-paneled plot
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Plot histogram of MedalCount2012 before log transformation
hist(data$Medal2012, 
     main = "Histogram of Medal Count in 2012 Olympics\n(Before Log Transformation)",  # Title of the histogram
     xlab = "Medal Count",                                # Label for x-axis
     ylab = "Frequency",                                  # Label for y-axis
     col = "skyblue",                                     # Color of bars
     border = "white"                                     # Color of border
)

# Plot histogram of MedalCount2012 after log transformation
hist(data$log_Medal2012, 
     main = "Histogram of Medal Count in 2012 Olympics\n(After Log Transformation)",  # Title of the histogram
     xlab = "Log Medal Count",                            # Label for x-axis
     ylab = "Frequency",                                  # Label for y-axis
     col = "skyblue",                                     # Color of bars
     border = "white"                                     # Color of border
)

# Print correlation matrix
print("Correlation matrix:After Log transform")
cor_matrix <- cor(data[,c("GDP", "Population", "Medal2012","log_Medal2012")])
print(cor_matrix)

# Plot correlation matrix with values
print("Plotting correlation matrix:")
corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")




par(mfrow = c(1, 2))
# Task 2: Linear regression model with log-transformed outputs
print("Task 2: Linear regression model with log-transformed outputs")
 # Adding 1 to avoid log(0)
lm_model2 <- lm(log_Medal2012 ~ Population + GDP, data = data)
print("Model Summary:")
print(summary(lm_model2))


# Plot regression line
print("Linear regression plot:")
plot_lm <- plot(lm_model2, which = 1)  # Specify which = 1 for the scatterplot of observed vs. fitted values

# Plot regression line
print("Residual plot:")
plot_residuals <- plot(lm_model2, which = 2)

# Plot regression line
print("QQ plot:")
plot_qq <- plot(lm_model2, which = 3)

# Plot regression line
print("Scale-location plot:")
plot_scale_location <- plot(lm_model2, which = 4)  # Using which = 4 for scale-location plot

# Arrange plots in a 2x2 grid
grid.arrange(plot_lm, plot_residuals, plot_qq, plot_scale_location, nrow = 2)


###Model Comparison:
# Create a data frame for observed and predicted values of both models
# Compute residuals and fitted values for both models
fitted_values_model1 <- fitted(lm_model)
residuals_model1 <- residuals(lm_model)

fitted_values_model2 <- fitted(lm_model2)
residuals_model2 <- residuals(lm_model2)

# Create separate residual plots for each model
plot_model1 <- ggplot(data.frame(Fitted_Values = fitted_values_model1, Residuals = residuals_model1), aes(x = Fitted_Values, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values - Model 1",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

plot_model2 <- ggplot(data.frame(Fitted_Values = fitted_values_model2, Residuals = residuals_model2), aes(x = Fitted_Values, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values - Model 2",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Display the plots
grid.arrange(plot_model1, plot_model2, ncol = 2)

# Task 3: Develop your own regression model
data$log_Population <- log(data$Population + 1)
data$log_GDP <- log(data$GDP + 1)

print("Task 3: Custom regression model")
# Fit polynomial regression model
poly_model <- lm(log_Medal2012 ~ poly(Population,2) + poly(GDP, 3), data = data)

# Print model summary
print(summary(poly_model))



# Plot regression line
print("Linear regression plot:")
print(plot(poly_model))



# Task 4: Model selection using AIC

# Set seed for reproducibility
set.seed(123)

# Split data into training (90%) and testing (10%) sets
train_index <- sample(1:nrow(data), 0.9 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit models on training data
lm_model_task1 <- lm(Medal2012 ~ Population + GDP, data = train_data)
lm_model_task2 <- lm(log_Medal2012 ~ Population + GDP, data = train_data)
lm_model_task3 <- lm(log_Medal2012 ~ poly(Population,2) + poly(GDP, 3), data = data)

# Predict on test data
pred_task1 <- predict(lm_model_task1, newdata = test_data)
pred_task2 <- exp(predict(lm_model_task2, newdata = test_data))  # Convert log scale back to original scale
pred_task3 <- exp(predict(lm_model_task3, newdata = test_data))  # Convert log scale back to original scale

# Calculate RMSE for each model
rmse_task1 <- sqrt(mean((test_data$Medal2012 - pred_task1)^2))
rmse_task2 <- sqrt(mean((exp(test_data$log_Medal2012) - pred_task2)^2))
rmse_task3 <- sqrt(mean((exp(test_data$log_Medal2012) - pred_task3)^2))

# Print RMSE for each model
cat("RMSE for Model Task 1:", rmse_task1, "\n")
cat("RMSE for Model Task 2:", rmse_task2, "\n")
cat("RMSE for Model Task 3:", rmse_task3, "\n")

par(mfrow = c(1, 1))
# RMSE values for each model
rmse_values <- c(rmse_task1, rmse_task2, rmse_task3)
models <- c("Model Task 1", "Model Task 2", "Model Task 3")

# Plotting the bar graph with center-aligned title
barplot(rmse_values, names.arg = models, 
        main = list("Root Mean Square Error (RMSE) by Model", cex = 1.5), 
        xlab = "Models", ylab = "RMSE", 
        col = "blue", border = "black", 
        ylim = c(0, max(rmse_values) + 1))


# Get the adjusted R-squared value for Model 1
adj_r_squared_model1 <- summary(lm_model_task1)$adj.r.squared

# Get the adjusted R-squared value for Model 2
adj_r_squared_model2 <- summary(lm_model_task2)$adj.r.squared

# Get the adjusted R-squared value for Model 3
adj_r_squared_model3 <- summary(lm_model_task3)$adj.r.squared

# Calculate AIC for each model
aic_task1 <- AIC(lm_model_task1)
aic_task2 <- AIC(lm_model_task2)
aic_task3 <- AIC(lm_model_task3)

# Compare AIC values
aic_values <- c(Task1 = aic_task1, Task2 = aic_task2, Task3 = aic_task3)
print(aic_values)

# Visualize AIC values
ggplot(data.frame(Model = names(aic_values), AIC = aic_values), aes(x = Model, y = AIC)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "AIC Comparison of Models",
       x = "Model",
       y = "AIC") +
  theme_minimal()

# Adjusted R-squared values and AIC scores for each model
# Adjusted R-squared values and AIC scores for each model
# Adjusted R-squared values and AIC scores for each model
adj_r_squared <- c(adj_r_squared_model1, adj_r_squared_model2, adj_r_squared_model3)
AIC_scores <- c(aic_task1, aic_task2, aic_task3)
models <- c("Model Task 1", "Model Task 2", "Model Task 3")
modelnames <- c("Model Task 1 - Base", "Model Task 2 - Log transformed output", "Model Task 3 - Custom Tuned")
modelnames
# Plotting the bar graph
par(mfrow = c(1, 2), mar = c(5, 8, 4, 2))  # Set up a 1x2 grid for side-by-side plots with wider margins
barplot(adj_r_squared, main = "Adjusted R-squared by Model", names.arg = models, col = "skyblue",
        ylim = c(0, max(adj_r_squared) + 0.1), ylab = "Adjusted R-squared")

# Add data labels for adjusted R-squared
text(x = barplot(adj_r_squared, plot = FALSE), y = adj_r_squared + 0.02, labels = round(adj_r_squared, 2), pos = 3)

barplot(AIC_scores, main = "AIC Scores by Model", names.arg = models, col = "salmon",
        ylim = c(0, max(AIC_scores) + 100), ylab = "AIC Score")

# Add data labels for AIC scores (rounded to one decimal place)
text(x = barplot(AIC_scores, plot = FALSE), y = AIC_scores + 10, labels = round(AIC_scores, 1), pos = 3)

# Set up a multi-paneled plot with three columns
par(mfrow = c(1, 3), mar = c(5, 8, 4, 2))  # Set wider margins for the multi-paneled plot


# Plot residuals vs. fitted values for Model 1
plot(lm_model_task1$fitted.values, residuals(lm_model_task1),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Model 1: Residuals vs Fitted Values",
     col = "blue", pch = 20)
abline(h = 0, col = "red", lwd = 2)

# Plot residuals vs. fitted values for Model 2
plot(lm_model_task2$fitted.values, residuals(lm_model_task2),
     xlab = "Fitted Values", ylab = "", # ylab left empty to save space
     main = "Model 2: Residuals vs Fitted Values",
     col = "green", pch = 20)
abline(h = 0, col = "red", lwd = 2)

# Plot residuals vs. fitted values for Model 3
plot(lm_model_task3$fitted.values, residuals(lm_model_task3),
     xlab = "Fitted Values", ylab = "", # ylab left empty to save space
     main = "Model 3: Residuals vs Fitted Values",
     col = "orange", pch = 20)
abline(h = 0, col = "red", lwd = 2)


par(mfrow = c(1, 3))
# Plot residuals vs. fitted values for Model 1
plot(fitted(lm_model_task1), residuals(lm_model_task1),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Model 1: Residuals vs Fitted Values",
     col = "blue", pch = 20)
abline(h = 0, col = "red", lwd = 2)
plot(fitted(lm_model_task2), residuals(lm_model_task2),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Model 2: Residuals vs Fitted Values",
     col = "green", pch = 20)
abline(h = 0, col = "red", lwd = 2)
plot(fitted(lm_model_task3), residuals(lm_model_task3),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Model 3: Residuals vs Fitted Values",
     col = "orange", pch = 20)
abline(h = 0, col = "red", lwd = 2)



# Normality of Residuals: Histogram with Normal Curve
par(mfrow = c(1, 3))
hist(residuals(lm_model_task1), probability = TRUE,
     main = "Model 1: Histogram of Residuals",
     xlab = "Residuals", ylab = "Density",
     col = "blue", border = "black")
curve(dnorm(x, mean = mean(residuals(lm_model_task1)), sd = sd(residuals(lm_model_task1))),
      add = TRUE, col = "red", lwd = 2)

# Normality of Residuals: Histogram with Normal Curve
hist(residuals(lm_model_task2), probability = TRUE,
     main = "Model 2: Histogram of Residuals ",
     xlab = "Residuals", ylab = "Density",
     col = "green", border = "black")
curve(dnorm(x, mean = mean(residuals(lm_model_task2)), sd = sd(residuals(lm_model_task2))),
      add = TRUE, col = "red", lwd = 2)

# Normality of Residuals: Histogram with Normal Curve
hist(residuals(lm_model_task3), probability = TRUE,
     main = "Model 3: Histogram of Residuals ",
     xlab = "Residuals", ylab = "Density",
     col = "orange", border = "black")
curve(dnorm(x, mean = mean(residuals(lm_model_task3)), sd = sd(residuals(lm_model_task3))),
      add = TRUE, col = "red", lwd = 2)


# Task 5: Compute the probability that the UK wins at least one medal given the estimated model parameters

print("Task 5: Computing the probability that the UK wins at least one medal")
# Extract data for Great Britain
UK_data <- data[data$Country == "Great Britain", ]

# Predict the number of medals for the UK using the model
UK_predicted_medals <- predict(lm_model_task3, newdata = UK_data)

# Calculate the probability of winning at least one medal
probability_at_least_one_medal <- ppois(1, lambda = UK_predicted_medals, lower.tail = FALSE)

# Output the probability
probability_at_least_one_medal






