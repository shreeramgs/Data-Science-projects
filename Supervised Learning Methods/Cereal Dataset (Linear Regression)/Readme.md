# Cereal Data Analysis
Here we consider cereal dataset to perform various data exploration, visualization, and modeling tasks using R


## Dependencies

Make sure you have the following R packages installed:

- caret
- olsrr
- leaps
- glmnet
- lattice

You can install these packages using the following command:

```R
install.packages(c("caret", "olsrr", "leaps", "glmnet", "lattice"))
```

## Data Exploration and Preprocessing

To uncover data set characteristics and initial patterns by using data visualization and statistical analysis

- Retrieving the dimension of the dataset
- Viewing the data type and the first few rows
- Generating statistical summaries
- Calculating means and standard deviations for different variables
- removing outliers

## Data Visualization

- Scatter plots
- Histograms
- Box plots and XY plots

## Linear Regression Modeling

 To predict the "rating" variable

 - Splitting the dataset into training and test sets
 - Fitting a linear regression model using the training data
 - Evaluating the model's performance on the test data using mean squared error (MSE)
 - Conducting stepwise forward regression and AIC forward regression to select predictors
 - Performing best subset selection to identify the optimal subset of predictors
