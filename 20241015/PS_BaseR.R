#20241015
# Problem Set 1: Base R

# Solution 1
library(MASS) # Load the library which includes Boston data set
attach(Boston) # Load the data set
?Boston # Get information about the data set
df=Boston # Load the data set
View(Boston) # View the data set
dim(df) # Dimensions of the data frame (506 observations, 14 variables)
ls(df) # List of variables of the data set

# crim -> per capita crime rate by town.
# zn -> proportion of residential land zoned for lots over 25,000 sq.ft.
# indus -> proportion of non-retail business acres per town.
# chas -> Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox -> nitrogen oxides concentration (parts per 10 million).
# rm -> average number of rooms per dwelling.
# age ->proportion of owner-occupied units built prior to 1940.
# dis -> weighted mean of distances to five Boston employment centres.
# rad -> index of accessibility to radial highways.
# tax -> full-value property-tax rate per $10,000.
# ptratio -> pupil-teacher ratio by town.
# black -> 1000(𝐵𝑘− 0.63)^2 where 𝐵𝑘is the proportion of blacks by town.
# lstat -> lower status of the population (percent).
# medv -> median value of owner-occupied homes in $1000s.

# The rows of dataset represent individual houses in Boston’s suburbs


# Solution 2
library(ggplot2)   # For plotting
library(corrplot)  # For correlation matrix visualization

cor_matrix <- cor(df) # Calculate the correlation matrix

# Display the correlation between MEDV, RM, and NOX
cor_medv_rm_nox <- cor_matrix["medv", c("rm", "nox")]
print(cor_medv_rm_nox)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", tl.cex = 0.8)

# Plot MEDV vs RM (average number of rooms per dwelling)
ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(color = "blue") +
  labs(title = "MEDV vs Average Number of Rooms (RM)",
       x = "Average Number of Rooms (RM)",
       y = "Median Value of Homes (MEDV)")

# Plot MEDV vs NOX (pollution level)
ggplot(Boston, aes(x = nox, y = medv)) +
  geom_point(color = "red") +
  labs(title = "MEDV vs Pollution (NOX)",
       x = "Nitric Oxides Concentration (NOX)",
       y = "Median Value of Homes (MEDV)")


# Interpretation of Graphs
#
# 1. medv vs. rm (Average Number of Rooms per Dwelling)
# 
# •	Observation: The plot shows a clear positive trend, where MEDV (median home value) tends to increase 
# as RM (average number of rooms per dwelling) increases.
# 
# •	Interpretation: This suggests that homes with more rooms are typically more valuable. 
# This trend is likely because larger homes tend to have higher property values. 
# The relationship here is relatively strong, as confirmed by the correlation coefficient (around 0.70), 
# indicating a fairly strong linear association.
# 
# 2. medv vs nox (Pollution Level)
# 
# •	Observation: In this plot, we see a negative trend, where MEDV generally decreases as NOX (pollution level) increases.
# 
# •	Interpretation: This implies that higher pollution areas tend to have lower home values.
# The negative correlation (approximately -0.43) suggests a moderate inverse relationship, meaning as pollution worsens,
# the desirability—and hence value—of homes in those areas tends to drop. This could be due to a variety of factors, 
# such as people’s preference for living in cleaner, healthier environments. 

# Solution 3
sum(df$chas) # 35 of the suburbs are bound the Charles river.

# Solution 4
median(df$ptratio) # The median pupil-teacher ratio among the towns is 19.05

# Solution 5
sum(df$rm > 7) # 64 of the areas average more than seven rooms per dwelling.
sum(df$rm > 8) # 13  of the areas average more than seven rooms per dwelling.

# Descriptive statistics for the suburbs that average more than eight rooms per dwelling.
library("dplyr") 
rm_8 <- df %>% 
  dplyr::filter(rm>8)
summary(rm_8)

# Solution 6

nox_upper_quantile <- quantile(df$nox,0.75) # Define upper quantile threshold

df$hp <- ifelse(df$nox > nox_upper_quantile, "Yes", "No") # Create a dummy variable "hp" indicating high pollution areas

dv_hp <- as.factor(df$hp)

plot(dv_hp,medv)


# 1.	Boxplot Structure:
# •	Each box represents the interquartile range (IQR) of MEDV for the two categories: 
#   “High Pollution” (Yes) and “Low Pollution” (No).
# •	The line inside each box indicates the median value of the homes for that category.
# •	The “whiskers” extend to the minimum and maximum values within 1.5 times the IQR from the quartiles.
# 2.	Median Values:
# •	The median value for low pollution areas (“No”) is visibly higher than that for high pollution areas (“Yes”). 
# This suggests that homes in areas with lower pollution levels tend to have a higher median property value.
# 3.	Interquartile Range:
# •	The box for low pollution areas is wider compared to the box for high pollution areas, 
# indicating greater variability in property values within low pollution areas.
# •	In contrast, the box for high pollution areas is narrower, suggesting that property values are 
# more consistent but generally lower.
# 4.	Outliers:
# •	If any points appear outside the whiskers, they are considered outliers. 
# These could indicate exceptionally high or low property values in a particular area.
# •	If present, outliers in the low pollution category may indicate some high-value properties 
# that are significantly above the typical range.
# 
# Overall Conclusion:
#   
# The boxplot clearly illustrates that areas with high pollution levels generally have lower median 
# property values compared to areas with low pollution levels. 
# This trend aligns with expectations, as higher pollution is often associated with less desirable living conditions, 
# which can lead to decreased property values. The greater variability in low pollution areas also suggests 
# that while they may have higher values, there are still some homes that are significantly more expensive, 
# contributing to a wider range of property values.


# Solution 7

# Calculate quartiles for NOX
nox_quantiles <- quantile(df$nox, probs = seq(0, 1, 0.25), na.rm = TRUE)
print(nox_quantiles)

# Create a numerical and categorical variable mapping nox onto quartiles
df$noxq <- cut(df$nox, breaks = nox_quantiles, include.lowest = TRUE, labels = 1:4)  # Numeric labels
df$noxq.f <- cut(df$nox, breaks = nox_quantiles, include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))  # Factor labels

# Graphically compare median property values across quartiles of NOX
# Boxplot using the numerical variable
ggplot(df, aes(x = factor(noxq), y = medv)) +
  geom_boxplot() +
  labs(title = "Median Home Value by NOX Quartile (Numerical)",
       x = "NOX Quartile (Numerical)",
       y = "Median Value of Homes (MEDV)")

# Boxplot using the categorical variable
ggplot(df, aes(x = noxq.f, y = medv)) +
  geom_boxplot() +
  labs(title = "Median Home Value by NOX Quartile (Categorical)",
       x = "NOX Quartile (Categorical)",
       y = "Median Value of Homes (MEDV)")


# Understanding the differences between these two variables can enhance your analytical approach 
# and interpretation of results. By considering both representations, you can gain a more nuanced 
# understanding of how NOX pollution relates to median home values, allowing for more comprehensive 
# conclusions and discussions based on the data.








