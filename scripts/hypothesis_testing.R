#install.packages("readxl")
#install.packages("corrplot")
#install.packages("car")
#install.packages("WRS2")

library(readxl) # for reading Excel files
library(ggplot2) # for data visualization
library(corrplot) # for visualising correlation matrix
library(car) # for Levene's test
library(WRS2)

#####INITIAL EXPLORATORY ANALYSIS#####

# Reading the data
data <- read_excel('concrete compressive strength.xlsx')

# View first rows
head(data)

# Structure of the dataset and data types
str(data)

# Summary statistics
summary(data)

# Checking for missing values
colSums(is.na(data))

# Dimension of the dataset
dim(data)


# Check for duplicate samples
duplicated_rows <- data[duplicated(data), ]
print(duplicated_rows)
# remove duplicates
data <- unique(data)


# Extract numerical variables
data_reduced <- data[, sapply(data, is.numeric)]

# Generate a correlation matrix for numerical variables
cor_matrix <- cor(data_reduced)

# Print the correlation matrix
print(cor_matrix)


# Visualise the correlation matrix using corrplot
corrplot(cor_matrix, method = "number", type = "upper", tl.cex = 0.7, number.cex = 0.7)

# Check for duplicate samples
duplicated_rows <- data[duplicated(data), ]
print(duplicated_rows)
# remove duplicates
data <- unique(data)


# pairplot of the numerical variables
pairs(data_reduced, lower.panel = NULL, pch = 19, cex = 0.2) # pairplot with scatter plots

# Check for outliers using boxplots
boxplot(data_reduced, outline = TRUE, col = "skyblue", border = "black")



# Create Histograms for Numeric Columns
lapply(names(data_reduced), function(var) {
  ggplot(data_reduced, aes_string(x = paste0("`", var, "`"))) + 
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    ggtitle(paste("Distribution of", var)) +
    theme_minimal()
})



#####Hypothesis Testing 1#####

# To test whether the mean concrete compressive strength for Coarse aggregate is greater than the mean for Fine aggregate.

# Null Hypothesis: The mean concrete compressive strength for Coarse aggregate is less than or equal to the mean for Fine aggregate.
# Alternative Hypothesis: The mean concrete compressive strength for Coarse aggregate is greater than the mean for Fine aggregate.

# Dependent Variable: Concrete Compressive Strength
# Independent Variable: Concrete Category (Categorical: Coarse, Fine)

# Separate the data into two groups based on the "Concrete Category" variable.

# Coarse Aggregate subset
coarse_data <- subset(data, `Concrete Category` == "Coarse")

# Fine Aggregate subset
fine_data <- subset(data, `Concrete Category` == "Fine")

# Extract the compressive strength column from each group
coarse_strength <- coarse_data$`Concrete compressive strength(MPa, megapascals)`
fine_strength <- fine_data$`Concrete compressive strength(MPa, megapascals)`

# Check the number of observations in each group
cat("Number of observations in Coarse group:", length(coarse_strength), "\n")
cat("Number of observations in Fine group:", length(fine_strength), "\n")

# Create histograms for each group to check for data distribution
hist(coarse_strength, breaks = 20, main = "Histogram of Coarse Aggregate Strength",
     xlab = "Concrete Compressive Strength (MPa)", col = "skyblue", border = "white")

hist(fine_strength, breaks = 20, main = "Histogram of Fine Aggregate Strength",
     xlab = "Concrete Compressive Strength (MPa)", col = "pink", border = "white")

# Q-Q plots to check for normality
qqnorm(coarse_strength, main = "Q-Q Plot for Coarse Aggregate Strength")
qqline(coarse_strength, col = "red")

qqnorm(fine_strength, main = "Q-Q Plot for Fine Aggregate Strength")
qqline(fine_strength, col = "red")

# Box plots to compare the spread (variance) of the two groups
boxplot(`Concrete compressive strength(MPa, megapascals)` ~ `Concrete Category`, data = data,
        main = "Boxplot of Concrete Compressive Strength by Category",
        xlab = "Concrete Category", ylab = "Concrete Compressive Strength (MPa)",
        col = c("skyblue", "pink"))

# Combine the data into a single data frame for Levene's Test
combined_data <- data.frame(
  strength = c(coarse_strength, fine_strength),
  category = rep(c("Coarse", "Fine"), c(length(coarse_strength), length(fine_strength)))
)

# Perform Levene's Test for equality of variances

levene_test <- leveneTest(strength ~ category, data = combined_data)
print(levene_test)

# Check the result of Levene's Test
if (levene_test$`Pr(>F)`[1] > 0.05) {
  cat("Levene's Test p-value:", levene_test$`Pr(>F)`[1], "\n")
  cat("Variances are equal. Performing standard t-test...\n")
  
  # Perform standard t-test (equal variances)
  t_test <- t.test(coarse_strength, fine_strength, alternative = "greater", var.equal = TRUE)
  
} else {
  cat("Levene's Test p-value:", levene_test$`Pr(>F)`[1], "\n")
  cat("Variances are not equal. Welch's t-test is more appropriate...\n")
  
  # Perform t-test with unequal variances (Welch's t-test)
  t_test <- t.test(coarse_strength, fine_strength, alternative = "greater", var.equal = FALSE)
}

# Display the results of the t-test
print(t_test)



#####Hypothesis Testing 2#####
# To test whether the presence of Fly Ash is associated with the Concrete Category.

# Null hypothesis: The presence of Fly Ash is independent of the Concrete Category.
# Alternative hypothesis: The presence of Fly Ash is associated with the Concrete Category.

# Dependent Variable: Contains Fly Ash (Categorical: TRUE, FALSE)
# Independent Variable: Concrete Category (Categorical: Coarse, Fine)

# We use a chi-squared test of independence to test the association between the two categorical variables.

# Create a contingency table of the two categorical variables
contingency_table <- table(data$`Concrete Category`, data$`Contains Fly Ash`)

# Display the contingency table
print(contingency_table)

# Perform a chi-squared test of independence
chi_test <- chisq.test(contingency_table)

# Display the results of the chi-squared test
print(chi_test)

# Visualise the proportion of Fly Ash presence by Concrete Category using a stacked bar chart
# Convert contingency table to a data frame for ggplot
plot_data <- as.data.frame(contingency_table)

# Create a stacked bar chart
ggplot(plot_data, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportion of Fly Ash Presence by Concrete Category",
       x = "Concrete Category", y = "Proportion",
       fill = "Fly Ash Presence") +
  theme_minimal()








#####Hypothesis Testing 3#####

#Dependent Variable: Concrete compressive strength
#Independent Variable (Factors influencing the dependent variable):Concrete Category (2 levels: Coarse, Fine) and Contains Fly Ash (2 levels: TRUE, FALSE)

# We will use a two-way ANOVA to test the effect of Concrete Category and Contains Fly Ash on the Concrete Compressive Strength.

# Hypotheses
# In Two way ANOVA there are 3 hypothesis to test because we are examining the effect of two factors on the dependent variable and the interaction between the two factors.

#Hypothesis for the effect of Concrete Category on Concrete Compressive Strength
#Null Hypothesis (H₀): The mean compressive strength is the same for Coarse and Fine categories.
#Alternative Hypothesis (H₁): The mean compressive strength is different for Coarse and Fine categories.

#Hypothesis for the effect of Contains Fly Ash on Concrete Compressive Strength
#Null Hypothesis (H₀): The mean compressive strength is the same for concrete samples with and without Fly Ash.
#Alternative Hypothesis (H₁): The mean compressive strength is different for concrete samples with and without Fly Ash.

# Hypothesis for the interaction Effect between Concrete Category and Contains Fly Ash on Concrete Compressive Strength
#Null Hypothesis (H₀): There is no interaction effect between Concrete Category and Contains Fly Ash presence on the compressive strength.
#Alternative Hypothesis (H₁): There is an interaction effect between Concrete Category and Contains Fly Ash presence on the compressive strength.


# Convert the predicting variables to factors for the analysis to ensure they are treated as categorical variables.
data$`Concrete Category` <- as.factor(data$`Concrete Category`)
data$`Contains Fly Ash` <- as.factor(data$`Contains Fly Ash`)


# create a two-way ANOVA model
model <- aov(`Concrete compressive strength(MPa, megapascals)` ~ 
               `Concrete Category` * `Contains Fly Ash`, data = data)


# Checking the ANOVA Assumptions

# Dependent variable should be continuous.
# Test passed as the dependent variable "Concrete compressive strength" is continuous.

# Independent variables should be categorical with two or more levels.
# Test passed as the independent variables "Concrete Category" and "Contains Fly Ash" are categorical with two levels each.
# Also the predicting variables have been converted to factors to ensure they are treated as categorical variables.



# Independence of observations. There is no relationship between the observations in each group or between groups.
# Test passed as the data is assumed to be collected independently.


# There should be no significant outliers in the data.
# Visualize outliers using boxplots to visually check for outliers
# in the dependent variable (Concrete compressive strength(MPa, megapascals)) 
# for each combination of the independent variables (Concrete Category and Contains Fly Ash):
ggplot(data, aes(x = interaction(`Concrete Category`, `Contains Fly Ash`), 
                 y = `Concrete compressive strength(MPa, megapascals)`)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  labs(title = "Boxplot to Identify Outliers",
       x = "Combination of Concrete Category and Fly Ash Presence",
       y = "Concrete Compressive Strength (MPa)") +
  theme_minimal()


# Dependent variable should be almost normally distributed for each combination of the levels of the independent variables.
# Shapiro-Wilk test for normality (Normality of residuals)
# Extract the residuals from the two-way ANOVA model
residuals <- residuals(model)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)


# Homogeneity of variances. The variance of the dependent variable should be equal
# across different levels of the independent variables.
# Levene's test for homogeneity of variances
levene_test <- leveneTest(`Concrete compressive strength(MPa, megapascals)` ~ 
                            `Concrete Category` * `Contains Fly Ash`, data = data)
print(levene_test)




# Addressing violations of ANOVA assumptions
# Shapiro-Wilk Test showed that residuals are not normally distributed (p-value = 2.584e-08, which is less than 0.05.), violating the normality assumption.
# Levene's Test showed that the variances are not equal across different levels of the independent variables (p-value = 7.028e-10, which is less than 0.05.), violating the homogeneity of variances assumption.
# Boxplot showed the presence of outliers in the Coarse.FALSE and Fine.TRUE groups.

# Apply log transformation to the dependent variable to address the non-normality issue and stabilize the variance by reducing the impact of outliers.
data$log_strength <- log(data$`Concrete compressive strength(MPa, megapascals)`)

# Build a new model with log-transformed dependent variable
model_log <- aov(log_strength ~ `Concrete Category` * `Contains Fly Ash`, data = data)


# Re-check assumptions after log transformation

# Check for outliers after log transformation
# Boxplot to check for outliers after log transformation
ggplot(data, aes(x = interaction(`Concrete Category`, `Contains Fly Ash`), 
                 y = log_strength)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  labs(title = "Boxplot to Identify Outliers After Log Transformation",
       x = "Combination of Concrete Category and Fly Ash Presence",
       y = "Log of Concrete Compressive Strength") +
  theme_minimal()


# Check for normality of residuals after log transformation
# Extract residuals
residuals_log <- residuals(model_log)
# Perform Shapiro-Wilk test
shapiro_test_log <- shapiro.test(residuals_log)
cat("Shapiro-Wilk Test for Normality (After Log Transformation):\n")
print(shapiro_test_log)

# Check for homogeneity of variances after log transformation
# Levene's test for homogeneity of variances with log-transformed variable
levene_test_log <- leveneTest(log_strength ~ `Concrete Category` * `Contains Fly Ash`, data = data)
cat("\nLevene's Test for Homogeneity of Variances (After Log Transformation):\n")
print(levene_test_log)



# Log transformation of the dependent variable has not improved the normality of residuals and homogeneity of variances.
# We will use a robust two-way ANOVA test to address the violations of ANOVA assumptions.
# The t2way function from the WRS2 package is used to perform a robust two-way ANOVA test.

# Perform robust two-way ANOVA
# log_strength is the dependent variable (transformed concrete strength)
# `Concrete Category` and `Contains Fly Ash` are the independent variables
robust_anova <- t2way(log_strength ~ `Concrete Category` * `Contains Fly Ash`, data = data)

# View the results
print(robust_anova)



# Visualising the log-transformed compressive strength across all group combinations
ggplot(data, aes(x = interaction(`Concrete Category`, `Contains Fly Ash`), 
                 y = log_strength, 
                 fill = `Contains Fly Ash`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log of Compressive Strength by Group",
       x = "Combination of Concrete Category and Fly Ash Presence",
       y = "Log of Concrete Compressive Strength (MPa)",
       fill = "Contains Fly Ash") +
  theme_minimal()









