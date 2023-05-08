library(caret)
library(olsrr)
library(leaps)
library(glmnet)
#Read the CSV file and store it in a variable an create class
var1 <- read.delim("C:/Users/Sriram/Downloads/cereal.csv",sep = ",")
var1

#Retrieve dimension var1
dim(var1)

#view var1 data type
class(var1)

#view the first few rows
head(var1)

#scatter plot
X11()
plot(var1$calories, var1$protien , main = "Protien - fiber analysis", xlab = "Fiber", ylab = "protien"	)

#statistical summary of data-frame
summary(var1)

#mean and standard deviation for all variables
#calories
calories_mean = mean(var1$calories)
calories_sd = sd(var1$calories)

#protein
protein_mean = mean(var1$protein)
protein_sd = sd(var1$protein)

#fat
fat_mean = mean(var1$fat)
fat_sd = sd(var1$fat)

#sodium
sodium_mean = mean(var1$sodium)
sodium_sd = sd(var1$sodium)

#fiber
fiber_mean = mean(var1$fiber)
fiber_sd = sd(var1$fiber)

#carbo
carbo_mean = mean(var1$carbo)
carbo_sd = sd(var1$carbo)

sugars_mean = mean(var1$sugars)
sugars_sd = sd(var1$sugars)

#vitamins
vitamins_mean = mean(var1$vitamins)
vitamins_sd =sd(var1$vitamins)

#shelf
shelf_mean =mean(var1$shelf)
shelf_sd =sd(var1$shelf)

#weight
weight_mean =mean(var1$weight)
weight_sd =sd(var1$weight)
#cups
cups_mean = mean(var1$cups)
cups_sd = sd(var1$cups)
#rating
rating_mean = mean(var1$rating)
rating_sd =sd(var1$rating)

#array of mean of all variables
x <- c(calories_mean, protein_mean,fat_mean,sodium_mean,fiber_mean,carbo_mean,vitamins_mean,shelf_mean,weight_mean,cups_mean,rating_mean)
x

#array of standard deviation of all variables
y <- c(calories_sd, protein_sd, fat_sd, sodium_sd, fiber_sd, carbo_sd, vitamins_sd, shelf_sd,weight_sd, cups_sd, rating_sd )
y

#normal distribution of mean and standard deviations from all the variables
z <-  dnorm(77, x, y)

#plot mean-sd
x11()
plot(x, y , main = "mean - standard deviation analysis", xlab = "mean", ylab = "standard deviation"	)


#histogram of fiber
x11()
hist(var1$fiber,
     xlab = "fiber",
     main = "Histogram of calories",
     breaks = sqrt(nrow(var1))
)

#histogram of array of mean of all variables
x11()
hist(x,
     xlab = "array of mean of all variables",
     main = "Histogram of calories",
     breaks = sqrt(nrow(var1))
)

#histogram of array of standard deviation of all variables
x11()
hist(y,
     xlab = "array of standard deviation of all variables",
     main = "Histogram of calories",
     breaks = sqrt(nrow(var1))
)

#boxplot of fiber
x11()
boxplot(var1$fiber,
        ylab = "fiber"
)

#removing outliers in fiber in box plot
outliers_variable <- boxplot.stats(var1$calories)$out
var1[which(var1$calories %in% outliers_variable),]
var2 <- var1[-which(var1$calories %in% outliers_variable),]
save(var2, file = "var2.RData")


#boxplot of array of mean of all variables
x11()
boxplot(x,
        ylab = "array of mean of all variables"
)


#boxplot of array of standard deviation of all variables
x11()
boxplot(y,
        ylab = "array of standard deviation of all variables"
)

#xyplot
x11()
library(lattice)
xyplot(x~rating | mfr , data = var1)

####
smp_size <- floor(0.8 * nrow(var2))
smp_size

## Set the seed to ensure reproducibility
set.seed(123)
## Randomly sample indices for training data
train_ind <- sample(seq_len(nrow(var2)), size = smp_size)
train <-var2[train_ind, ]
test <- var2[-train_ind, ]
#view train and test splits
test
train

#Fitting a linear regression model to predict 'rating'
model=lm(rating~.,data=train)
#statistical summary linear regression 
summary(model)

#here we are fitting training model onto the test set
pred=predict(model,test)
#Now using mean squared error, we are Calculating the Accuracy
MSE=mean((test$rating-pred)^2)
#Printing MSE
print(MSE)

#Stepwise Forward Regression
forward <- lm(rating~sodium+protein+fat,data=train)
forward_fit <- ols_step_forward_p(forward, penter = 0.2)
forward_fit

#Stepwisecaic(Akaike Information Criteria) forward regression
akaike <- lm(rating~sodium+protein+fat,data=train)
akaike_fit <- ols_step_forward_aic(forward, penter = 0.2)
akaike_fit
plot(akaike_fit)

#Select the subset of predictors
exhaustive <-lm(rating~sodium+protein+fat,data=train)
exhaustive_fit<- ols_step_best_subset(exhaustive, details = TRUE)
exhaustive_fit
plot(exhaustive_fit)

