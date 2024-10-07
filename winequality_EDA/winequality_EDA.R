wine_quality <- read.csv("winequality.csv")

# Display the first few rows of the dataset
head(wine_quality)

# Check the data types of each column
str(wine_quality)

column_types <- sapply(wine_quality, class)
print(column_types)

# Calculate basic summary statistics for each column
summary_stats <- summary(wine_quality)
print(summary_stats)

# Remove rows with any missing values
wine_quality <- na.omit(wine_quality)
summary <- summary(wine_quality)
print(summary)

# Load the dplyr package
library(dplyr)

# Remove duplicate rows
wine_quality_unique <- wine_quality %>%
  distinct()
wine_quality <- wine_quality_unique
# Load the forcats package
library(forcats)

# Encode categorical variables (assuming "type" is a categorical variable)
wine_quality$type <- factor(wine_quality_unique$type)
# Re-encode "type" variable with specific levels
wine_quality$type <- fct_relevel(wine_quality_unique$type, "red", "white")

# Min-max normalization for the "quality" variable
wine_quality$quality <- (wine_quality_unique$quality - min(wine_quality_unique$quality)) / (max(wine_quality_unique$quality) - min(wine_quality_unique$quality))

# Create a boxplot
ggplot(wine_quality, aes(y = quality)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Quality",
       y = "Quality") +
  theme_minimal()

# Create a histogram before outlier removal
ggplot(wine_quality, aes(x = quality)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Quality (Before Outlier Removal)",
       x = "Quality",
       y = "Frequency") +
  theme_minimal()

# Calculate the interquartile range (IQR)
Q1 <- quantile(wine_quality$quality, 0.25)
Q3 <- quantile(wine_quality$quality, 0.75)
IQR <- Q3 - Q1

# Define the lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- wine_quality$quality < lower_bound | wine_quality$quality > upper_bound

# Remove outliers
wine_quality <- wine_quality[!outliers, ]
head(wine_quality)

# Load required libraries
library(ggplot2)

# Create a histogram after outlier removal
ggplot(wine_quality[!outliers, ], aes(x = quality)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Quality (After Outlier Removal)",
       x = "Quality",
       y = "Frequency") +
  theme_minimal()

# Load required libraries
library(ggplot2)
library(dplyr)

# Select only numeric variables
numeric_variables <- select_if(wine_quality, is.numeric)

# Convert the selected variables to long format
wine_quality_long <- pivot_longer(numeric_variables, everything(), names_to = "variable", values_to = "value")

# Create histograms for all numerical variables in one plot
ggplot(data = wine_quality_long, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Variables", x = "Value", y = "Frequency") +
  theme_minimal()

categorical_variables <- select_if(wine_quality, is.factor)
# Create bar charts for categorical variables
bar_charts <- lapply(names(categorical_variables), function(var) {
  ggplot(data = wine_quality, aes_string(x = var)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = paste("Bar Chart of", var),
         x = var,
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})
print(bar_charts)

# Load the forcats package
library(forcats)

# Convert "type" variable to numeric using label encoding
wine_quality$type <- as.numeric(fct_recode(wine_quality$type, "1" = "red", "2" = "white"))


# Compute the correlation matrix
correlation_matrix <- cor(wine_quality, use = "complete.obs")

# Convert the correlation matrix to a long format
correlation_long <- melt(correlation_matrix)

# Plot the heatmap
ggplot(data = correlation_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "skyblue", mid = "white", high = "red", midpoint = 0,
                       name = "Correlation",
                       limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap of Variables",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Compute the correlation matrix
correlation_matrix <- cor(wine_quality, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Create scatter plots
pairs(wine_quality)

# Load required libraries
library(ggplot2)
library(gridExtra)

# Create the first scatter plot
scatter_plot1 <- ggplot(wine_quality, aes(x = pH, y = alcohol, color = type)) +
  geom_point() +
  labs(title = "Scatter Plot of Type vs Alcohol",
       x = "pH",
       y = "Alcohol") +
  theme_minimal()

# Create the second scatter plot
scatter_plot2 <- ggplot(wine_quality, aes(x = pH, y = fixed.acidity, color = type)) +
  geom_point() +
  labs(title = "Scatter Plot of Type vs Fixed Acidity",
       x = "pH",
       y = "Fixed Acidity") +
  theme_minimal()

# Create the third scatter plot
scatter_plot3 <- ggplot(wine_quality, aes(x = pH, y = residual.sugar, color = type)) +
  geom_point() +
  labs(title = "Scatter Plot of Type vs Residual Sugar",
       x = "pH",
       y = "Residual Sugar") +
  theme_minimal()

# Create the fourth scatter plot
scatter_plot4 <- ggplot(wine_quality, aes(x = pH, y = citric.acid, color = type)) +
  geom_point() +
  labs(title = "Scatter Plot of Type vs Citric Acid",
       x = "pH",
       y = "Citric Acid") +
  theme_minimal()

# Combine the plots
combined_plot <- grid.arrange(scatter_plot1, scatter_plot2, scatter_plot3, scatter_plot4, nrow = 2)

# Display the combined plot
print(combined_plot)

# Create interaction terms
wine_quality$interaction_term <- wine_quality$alcohol * wine_quality$sulphates
wine_quality$interaction_term

# Fit a linear regression model
lm_model <- lm(quality ~ ., data = wine_quality)
summary(lm_model)


# Load required libraries
library(ggplot2)

# Create line chart for pH
line_chart_pH <- ggplot(wine_quality, aes(x = pH, group = type, color = type)) +
  geom_density() +
  labs(title = "Density Plot of pH by Wine Type",
       x = "pH",
       y = "Density") +
  theme_minimal()

# Create line chart for alcohol
line_chart_alcohol <- ggplot(wine_quality, aes(x = alcohol, group = type, color = type)) +
  geom_density() +
  labs(title = "Density Plot of Alcohol by Wine Type",
       x = "Alcohol",
       y = "Density") +
  theme_minimal()

# Create line chart for fixed acidity
line_chart_fixed_acidity <- ggplot(wine_quality, aes(x = fixed.acidity, group = type, color = type)) +
  geom_density() +
  labs(title = "Density Plot of Fixed Acidity by Wine Type",
       x = "Fixed Acidity",
       y = "Density") +
  theme_minimal()

# Combine the line charts
combined_line_charts <- cowplot::plot_grid(line_chart_pH, line_chart_alcohol, line_chart_fixed_acidity, nrow = 3)

# Display the combined line charts
print(combined_line_charts)


# Load required libraries
library(ggplot2)

# Create scatter plot for pH vs alcohol with trend lines
scatter_plot_pH_alcohol <- ggplot(wine_quality, aes(x = pH, y = alcohol, color = type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear trend lines
  labs(title = "Scatter Plot of pH vs Alcohol by Wine Type",
       x = "pH",
       y = "Alcohol") +
  theme_minimal()

# Create scatter plot for fixed acidity vs alcohol with trend lines
scatter_plot_fixed_acidity_alcohol <- ggplot(wine_quality, aes(x = fixed.acidity, y = alcohol, color = type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear trend lines
  labs(title = "Scatter Plot of Fixed Acidity vs Alcohol by Wine Type",
       x = "Fixed Acidity",
       y = "Alcohol") +
  theme_minimal()

# Create scatter plot for residual sugar vs alcohol with trend lines
scatter_plot_residual_sugar_alcohol <- ggplot(wine_quality, aes(x = residual.sugar, y = alcohol, color = type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear trend lines
  labs(title = "Scatter Plot of Residual Sugar vs Alcohol by Wine Type",
       x = "Residual Sugar",
       y = "Alcohol") +
  theme_minimal()

# Combine the scatter plots
combined_scatter_plots <- cowplot::plot_grid(scatter_plot_pH_alcohol, scatter_plot_fixed_acidity_alcohol, scatter_plot_residual_sugar_alcohol, nrow = 3)

# Display the combined scatter plots
print(combined_scatter_plots)
