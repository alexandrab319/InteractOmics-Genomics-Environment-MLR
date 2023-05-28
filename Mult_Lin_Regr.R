# Install packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("caret")
install.packages("MASS")
install.packages("glmnet")

# Load packages
library(dplyr)
library(tidyr)
library(caret)
library(MASS)
library(glmnet)

# Read dataset
my_data <- read.csv("C:/Users/alexb/Downloads/096730_project2!.csv", header = TRUE)

# View first few rows of the dataset
head(my_data)

# Check dataset for missing or erroneous data
summary(my_data)
str(my_data)

# Fit a model with only the environmental variables
M_E <- lm(Y ~ E1+E2+E3+E4, data = my_data)

# Print the summary of the model
summary(M_E)

# Print the adjusted R Squared for the original model
summary(M_E)$adj.r.squared

# Fit the linear model with all variables squared
M_raw_sq <- lm(Y ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data=my_data)

# View the summary of the squared model
summary(M_raw_sq)

 # Create a residual plot for the squared model
plot(resid(M_raw_sq) ~ fitted(M_raw_sq), main='Residual Plot for Squared Model')

#add horizontal line at 0
abline(h = 0, lty = 2)

# Fit the initial model
M_raw <- lm(Y ~ E1 + E2 + E3 + E4 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16 + G17 + G18 + G19 + G20, data = my_data)

# Perform a Box-Cox transformation on the original model
library(MASS)
boxcox(M_raw_sq, lambda = seq(0.1, 3, by = 0.1))

LAMBDA <- boxcox(M_raw_sq, lambda = seq(-5, 5, length = 100), plotit = FALSE)
optimal_lambda <- LAMBDA$x[which.max(LAMBDA$y)]

#Applied optimal lambda 2.20 to the data 
M_trans <- lm( I(Y^2.20) ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data=my_data)

#Computed the R Squared value for original model
summary(M_raw)$adj.r.square

#Computed the R Squared value for squared model
summary(M_raw_sq)$adj.r.square

#Computed the R Squared value for the transformed model
summary(M_trans)$adj.r.square

#Plot the transformed residual plot
plot(resid(M_trans) ~ fitted(M_trans), main='New Residual Plot')

library(glmnet)

# Prepare data
X <- model.matrix(M_main)
Y <- I(my_data$Y^2.20)

# Perform cross-validation to select optimal lambda
cv_fit <- cv.glmnet(X, Y, alpha = 1, nfolds = 10)

# Plot the cross-validation results
plot(cv_fit)

# Select the optimal lambda based on cross-validation
lambda_optimal <- cv_fit$lambda.min

# Fit the Lasso model using the optimal lambda
lasso_fit <- glmnet(X, Y, alpha = 1, lambda = lambda_optimal)

# Print the coefficients of the Lasso model
print(coef(lasso_fit))


#####Stepwise Regression ##### MODEL 2 

# Load the 'leaps' package
install.packages("leaps")
library(leaps)

# Fit a forward stepwise regression model with up to 5 predictor variables
# Use the transformed outcome variable from the previous model, and exclude the intercept column from the model matrix
Y_trans <- (my_data$Y^2.20) # Transform the outcome variable (example transformation)
M_trans <- lm(I(Y^2.20) ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data=my_data) # Fit the transformed model
M <- regsubsets(model.matrix(M_trans)[,-1], Y_trans, nbest=1, nvmax=5, method='forward', intercept=TRUE)
temp <- summary(M)

# Load the knitr package
library(knitr)

# Extract the variable names from the model matrix
Var <- colnames(model.matrix(M_trans))

# Create a table with the selected model, adjusted R-squared, and BIC for each model size
M_select <- apply(temp$which, 1, function(x) paste0(Var[x], collapse='+'))
model_summary <- data.frame(cbind(model = M_select, adjR2 = temp$adjr2, BIC = temp$bic))

# Print the table using kable with a caption
kable(model_summary, caption = 'Model Summary')

# Fit the linear regression model
M_main <- lm(I(Y^2.20) ~ E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20, data=my_data)

# Select significant coefficients
temp <- summary(M_main)

# Print the table using kable with a caption
library(knitr)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')

# Fit a linear model with squared terms of E4, G19, and G20 as predictors
M_2stage <- lm(I(Y^2.20) ~ (E4 + G19 + G20)^2, data = my_data)

# Compute the summary of the model
temp <- summary(M_2stage)

# Extract the coefficients with absolute t-value >= 4
coeffs <- temp$coefficients[ abs(temp$coefficients[,3]) >= 4, ]

# Print a table of the significant coefficients, including the intercept
kable(coeffs, caption = 'Significant Coefficients')

# Summary values for the chosen final model
M_final <- lm(I(Y^2.20) ~ E4 + G19:G20, data = my_data)
summary(M_final)
anova(M_final)

##### Scatter plots Y vs. E4, G19, G20

#T - Test Y vs. G19
t_test <- t.test(Y ~ G19, my_data)
t_test

library(ggplot2)

# Scatter plot
ggplot(my_data, aes(x=G19, y=Y)) +
  geom_point() +
  ggtitle("Scatterplot of Y vs. G19") +
  xlab("G19") +
  ylab("Y")

# T Test Y vs. G20
t_test <- t.test(Y ~ G20, my_data)
t_test

# Scatter Plot
ggplot(my_data, aes(x=G20, y=Y)) +
  geom_point() +
  ggtitle("Scatterplot of Y vs. G20") +
  xlab("G20") +
  ylab("Y")

# Calculate correlation
correlation <- cor(my_data$Y, my_data$E4)
correlation

# Scatter plot Y vs. E4 
ggplot(my_data, aes(x=E4, y=Y)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("Scatterplot of Y vs. E4") +
  xlab("E4") +
  ylab("Y")

