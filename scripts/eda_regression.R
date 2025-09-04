#install.packages("readxl")
#install.packages("corrplot")
#install.packages("car")
#install.packages("WRS2")
#install.packages("leaps")

library(readxl) # for reading Excel files
library(ggplot2) # for data visualization
library(corrplot) # for visualising correlation matrix
library(car) # for Levene's test
library(WRS2) # for robust ANOVA
library(leaps) # for stepwise selection



#####INITIAL EXPLORATORY DATA ANALYSIS#####

# Reading the data
data <- read_excel('concrete compressive strength.xlsx')

# Dimension of the dataset
dim(data)

# Structure of the dataset and data types
str(data)

# View first rows
print(head(setNames(data, substr(colnames(data), 1, 10))))

# tail of the data
print(tail(setNames(data, substr(colnames(data), 1, 10))))

# Structure of the dataset and data types
str(data)

# Summary statistics
summary_stats <- as.data.frame(summary(data))
print(summary_stats)

# Checking for missing values
missing_values <- colSums(is.na(data))
missing_values_df <- as.data.frame(missing_values)
rownames(missing_values_df) <- colnames(data)
print(missing_values_df)

# Number of unique values in each column
unique_values <- sapply(data, function(x) length(unique(x)))
unique_values_df <- as.data.frame(unique_values)
print(unique_values_df)

# Count number of duplicate rows
num_duplicates <- nrow(duplicated_rows_df)
cat("Number of duplicate rows:", num_duplicates, "\n")

# Extract numerical variables
data_reduced <- data[, sapply(data, is.numeric)]

# Generate a correlation matrix for numerical variables
cor_matrix <- cor(data_reduced)

# Print the correlation matrix
print(cor_matrix)

# Visualise the correlation matrix using corrplot
corrplot(cor_matrix, method = "number", type = "upper", tl.cex = 0.7, number.cex = 0.7)

# Print correlations of predictors with the response variable
response_var <- "Concrete compressive strength(MPa, megapascals)"
correlations_with_response <- cor_matrix[, response_var, drop = FALSE]
print(correlations_with_response)

# pairplot of the numerical variables to check for linearity
pairs(data_reduced, lower.panel = NULL, pch = 19, cex = 0.2) # pairplot with scatter plots

# Check for outliers using boxplots
colnames(data_reduced) <- substr(colnames(data_reduced), 1, 10)
boxplot(data_reduced, outline = TRUE, col = "skyblue", border = "black", 
        las = 2,
        cex.axis = 0.8)

# Create Histograms for Numeric Columns to check for distribution
lapply(names(data_reduced), function(var) {
  ggplot(data_reduced, aes_string(x = paste0("`", var, "`"))) + 
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    ggtitle(paste("Distribution of", var)) +
    theme_minimal()
})


#####DATA PREPROCESSING#####


# Remove duplicates
data <- unique(data)

# Create dummy variables for predictors with dominant zeros (These are optional components in a concrete mixture)
# The original variable and its dummy varaible should nor be used in the same model as they have high multicollinearity.
data$Blast_Furnace_Slag_Present <- ifelse(data$`Blast Furnace Slag (component 2)(kg in a m^3 mixture)` > 0, 1, 0)
data$Fly_Ash_Present <- ifelse(data$`Fly Ash (component 3)(kg in a m^3 mixture)` > 0, 1, 0)
data$Superplasticizer_Present <- ifelse(data$`Superplasticizer (component 5)(kg in a m^3 mixture)` > 0, 1, 0)

# Log-transform variables with dominant zeros
data$`Blast Furnace Slag (component 2)(kg in a m^3 mixture)` <- ifelse(
  data$`Blast Furnace Slag (component 2)(kg in a m^3 mixture)` > 0,
  log1p(data$`Blast Furnace Slag (component 2)(kg in a m^3 mixture)`),
  0
)
data$`Fly Ash (component 3)(kg in a m^3 mixture)` <- ifelse(
  data$`Fly Ash (component 3)(kg in a m^3 mixture)` > 0,
  log1p(data$`Fly Ash (component 3)(kg in a m^3 mixture)`),
  0
)
data$`Superplasticizer (component 5)(kg in a m^3 mixture)` <- ifelse(
  data$`Superplasticizer (component 5)(kg in a m^3 mixture)` > 0,
  log1p(data$`Superplasticizer (component 5)(kg in a m^3 mixture)`),
  0
)

# Log-transform the remaining skewed variable (Age)
data$`Age (day)` <- log1p(data$`Age (day)`)

# Check the distributions of predictors after transformation
transformed_vars <- c(
  "Blast Furnace Slag (component 2)(kg in a m^3 mixture)",
  "Fly Ash (component 3)(kg in a m^3 mixture)",
  "Superplasticizer (component 5)(kg in a m^3 mixture)",
  "Age (day)"
)
lapply(transformed_vars, function(var) {
  ggplot(data, aes_string(x = paste0("`", var, "`"))) + 
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    ggtitle(paste("Distribution of", var, "After Transformation")) +
    theme_minimal()
})


# Generate updated correlation matrix for numerical variables
data_reduced <- data[, sapply(data, is.numeric)]
cor_matrix_updated <- cor(data_reduced)
# Visualize the updated correlation matrix
corrplot(cor_matrix_updated, method = "number", type = "upper", tl.cex = 0.7, number.cex = 0.7)

# Print correlations of predictors with the response variable
response_var <- "Concrete compressive strength(MPa, megapascals)"
correlations_with_response <- cor_matrix_updated[, response_var, drop = FALSE]
print(correlations_with_response)





#####MODELLING AND EVALUATION#####



#####Simple Linear Regression Model#####

# To predict the concrete compressive strength using the amount of cement as a predictor variable.

# Fit a simple linear regression model with cement as predictor variable
model_slr <- lm(`Concrete compressive strength(MPa, megapascals)` ~ `Cement (component 1)(kg in a m^3 mixture)`, 
                data = data)

# Display the summary of the model
summary(model_slr)

# Visualising the regression line
# Scatter plot with regression line
plot(data$`Cement (component 1)(kg in a m^3 mixture)`, 
     data$`Concrete compressive strength(MPa, megapascals)`,
     col = "blue", main = "Regression: Compressive Strength vs Cement",
     xlab = "Cement Content (kg/m³)", ylab = "Compressive Strength (MPa)")
abline(model, col = "red")

# Add the regression line to the plot
abline(model_slr, col = "red", lwd = 2) 


# Check the assumptions of the linear regression model

# Check linearity assumption
plot (model_slr, 1)

# Check residuals independence
plot (model_slr, 2)

# Check homoscedasticity (Equal variance of residuals)
plot (model_slr, 3)







#####Multiple Linear Regression Model#####

# To predict the concrete compressive strength using multiple predictor variables.

# Fit initial model with Cement and Age as predictor variables
model_mlr <- lm(`Concrete compressive strength(MPa, megapascals)` ~ 
                  `Cement (component 1)(kg in a m^3 mixture)` + 
                  `Age (day)`, data = data)
# Display summary for Model 1
summary(model_mlr)

# Add Superplasticizer as the third predictor
model_mlr2 <- lm(`Concrete compressive strength(MPa, megapascals)` ~ 
                   `Cement (component 1)(kg in a m^3 mixture)` + 
                   `Age (day)` + 
                   `Superplasticizer (component 5)(kg in a m^3 mixture)`, data = data)

# Display summary for model 2
summary(model_mlr2)

# Perform ANOVA tests between models 1 and 2
anova_model_1_2 <- anova(model_mlr, model_mlr2)
cat("ANOVA between Model 1 and Model 2:\n")
print(anova_model_1_2)


# Add Water as the fourth predictor
model_mlr3 <- lm(`Concrete compressive strength(MPa, megapascals)` ~ 
                   `Cement (component 1)(kg in a m^3 mixture)` + 
                   `Age (day)` + 
                   `Superplasticizer (component 5)(kg in a m^3 mixture)` + 
                   `Water  (component 4)(kg in a m^3 mixture)`, data = data)
# Display summary for model 3
summary(model_mlr3)

# Perform ANOVA tests between models 2 and 3
anova_model_2_3 <- anova(model_mlr2, model_mlr3)
cat("\nANOVA between Model 2 and Model 3:\n")
print(anova_model_2_3)


# Add Blast_Furnace_Slag_Present as the fifth predictor
model_mlr4 <- lm(`Concrete compressive strength(MPa, megapascals)` ~ 
                   `Cement (component 1)(kg in a m^3 mixture)` + 
                   `Age (day)` + 
                   `Superplasticizer (component 5)(kg in a m^3 mixture)` + 
                   `Water  (component 4)(kg in a m^3 mixture)` +
                   `Blast_Furnace_Slag_Present`, data = data)
# Display summary for model 4
summary(model_mlr4)

# Perform ANOVA tests between models 3 and 4
anova_model_3_4 <- anova(model_mlr3, model_mlr4)
cat("\nANOVA between Model 3 and Model 4:\n")
print(anova_model_3_4)


# Add Fine Aggregate as the sixth predictor
model_mlr5 <- lm(`Concrete compressive strength(MPa, megapascals)` ~ 
                   `Cement (component 1)(kg in a m^3 mixture)` + 
                   `Age (day)` + 
                   `Superplasticizer (component 5)(kg in a m^3 mixture)` + 
                   `Water  (component 4)(kg in a m^3 mixture)` +
                   `Blast_Furnace_Slag_Present` +
                   `Fine Aggregate (component 7)(kg in a m^3 mixture)`, data = data)
# Display summary for model 5
summary(model_mlr5)


# The adjusted R-squared value did not increase significantly after adding Fine Aggregate as a predictor.
# We therefore stop adding more predictors to the model and consider the model with Cement, 
# Age, Superplasticizer, water, and Blast Furnace Slag Present as the final model.
# However, we can programmatically explore all the possible subsets of these predictors 
# to try to reduce model's complexity and perhaps improve the model's performance.
# We use the function regsubsets from the leaps package to perform exhaustive search 
# of all possible subsets of predictors and select the best model based on the adjusted R-squared value.
# Perform exhaustive search
best_subsets <- regsubsets(`Concrete compressive strength(MPa, megapascals)` ~ 
                             `Age (day)` + 
                             `Cement (component 1)(kg in a m^3 mixture)` + 
                             `Superplasticizer (component 5)(kg in a m^3 mixture)` + 
                             `Blast_Furnace_Slag_Present` + 
                             `Water  (component 4)(kg in a m^3 mixture)`, 
                           data = data, nbest = 1, method = "exhaustive")

# Extract the best model by Adjusted R²
best_model_summary <- summary(best_subsets)
best_model_index <- which.max(best_model_summary$adjr2)

# Extract selected predictors
selected_predictors <- names(coef(best_subsets, best_model_index))[-1]

# Check if any predictors were selected
if (length(selected_predictors) == 0) {
  best_model_formula <- as.formula("`Concrete compressive strength(MPa, megapascals)` ~ 1")
} else {
  best_model_formula <- as.formula(
    paste("`Concrete compressive strength(MPa, megapascals)` ~", 
          paste(selected_predictors, collapse = " + "))
  )
}

# Refit and summarize the best model
best_model_refit <- lm(best_model_formula, data = data)
summary(best_model_refit)










######MODEL DIAGNOSTICS#####


# Check linearity assumption (Ensure that the relationship between predictors (Xs) and the response variable (Y) is linear.)
data_temp <- data
colnames(data_temp)[colnames(data_temp) %in% c("Concrete compressive strength(MPa, megapascals)",
                                               "Cement (component 1)(kg in a m^3 mixture)",
                                               "Age (day)",
                                               "Superplasticizer (component 5)(kg in a m^3 mixture)",
                                               "Water  (component 4)(kg in a m^3 mixture)",
                                               "Blast Furnace Slag (component 2)(kg in a m^3 mixture)")] <- 
  c("Strength", "Cement", "Age", "Superplasticizer", "Water", "Slag")

pairs(data_temp[, c("Strength", "Cement", "Age", "Superplasticizer", "Water", "Slag")], 
      lower.panel = NULL, pch = 19, cex = 0.2)


# Check residuals independence (There should be no pattern in the residuals vs. fitted values plot.)
plot(model_mlr4, 1)

# Check normality of residuals (Residuals should be normally distributed.)
plot(model_mlr4, 2)

# Check homoscedasticity (Equal variance of residuals)
plot(model_mlr4, 3)

# No multicollinearity (Predictor variables should not be highly correlated with each other.)
# Check for multicollinearity using the VIF (Variance Inflation Factor)
vif(model_mlr4)






# We found violation of linearity assumption in the model. We can address this 
# by adding interaction terms between the predictors.
# Generates interaction terms programmatically using the stepwise selection 
# method to include only significant interactions in the model.

# Standardise numeric predictors before using the interaction terms. 
data$Cement_scaled <- scale(data$`Cement (component 1)(kg in a m^3 mixture)`, center = TRUE, scale = TRUE)
data$Age_scaled <- scale(data$`Age (day)`, center = TRUE, scale = TRUE)
data$Superplasticizer_scaled <- scale(data$`Superplasticizer (component 5)(kg in a m^3 mixture)`, 
                                      center = TRUE, scale = TRUE)
data$Water_scaled <- scale(data$`Water  (component 4)(kg in a m^3 mixture)`, center = TRUE, scale = TRUE)

# Fit a model with all interactions using scaled predictors
interaction_model <- lm(`Concrete compressive strength(MPa, megapascals)` ~ 
                          (Cement_scaled *
                             Age_scaled *
                             Superplasticizer_scaled *
                             Water_scaled *
                             Blast_Furnace_Slag_Present),
                        data = data)

# Perform stepwise selection to include only significant interactions
interaction_stepwise <- step(interaction_model, direction = "both", trace = TRUE)

# View the summary of the selected model
summary(interaction_stepwise)


# Linearity checks
plot(data$Cement_scaled, residuals(interaction_stepwise))
abline(h = 0, col = "red")

plot(data$Age_scaled, residuals(interaction_stepwise))
abline(h = 0, col = "red")

plot(data$Superplasticizer_scaled, residuals(interaction_stepwise))
abline(h = 0, col = "red")

plot(data$Water_scaled, residuals(interaction_stepwise))
abline(h = 0, col = "red")

plot(data$Blast_Furnace_Slag_Present, residuals(interaction_stepwise))
abline(h = 0, col = "red")

# Check residuals independence (There should be no pattern in the residuals vs. fitted values plot.)
plot(interaction_stepwise, 1)

# Check normality of residuals (Residuals should be normally distributed.)
plot(interaction_stepwise, 2)

# Check homoscedasticity (Equal variance of residuals)
plot(interaction_stepwise, 3)

# No multicollinearity (Predictor variables should not be highly correlated with each other.)
# Check for multicollinearity using the VIF (Variance Inflation Factor)
vif(interaction_stepwise, type = "predictor")
