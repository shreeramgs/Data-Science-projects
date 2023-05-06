#Read the CSV file and store it in a variable an create class
var1 <- read.delim("C:/Users/Sriram/Downloads/cereal.csv",sep = ",")
var1
dim(var1)
class(var1)
#q1:

#scatter plot
X11()
plot(var1$calories, var1$protien , main = "Protien - fiber analysis", xlab = "Fiber", ylab = "protien"	)

#summary of all dataset
 a <- summary(var1)
 
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




#q2
var2
lm(rating ~., data=var2)
linear = lm(rating ~., data=var1)
summary(linear)
calories_fiber <-lm(rating ~calories*fiber, data=var2)
summary(calories_fiber)
#therefore calories*fiber with respect to response variable rating is insignificant

#q3
library(MASS)
head(Boston)
dim(Boston)
#q3 a)
str(Boston)
Boston$chas <- as.numeric(Boston$chas)
Boston$rad <- as.numeric(Boston$rad)
pairs(Boston)
# q3 b) the correlation coefficients with respect their corresponding p-values shows that there is a relation between the per-capita crime rate and the other predictors
#q3 c) 
summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)
library(ggplot2)
qplot(Boston$crim, binwidth=5 , xlab = "Crime rate", ylab="Number of Suburbs" )
qplot(Boston$tax, binwidth=50 , xlab = "Full-value property-tax rate per $10,000", ylab="Number of Suburbs")
qplot(Boston$ptratio, binwidth=5, xlab ="Pupil-teacher ratio by town", ylab="Number of Suburbs")
#so from the median(0.26%) and max crime rate(89%) values, we realize that there are few neighborhoods where crime rates are high
R<- subset( Boston, crim > 50)
nrow(R)/ nrow(Boston)
#from the above operations, we realize that there are 0.79% of the neighbohrhoods that have half(50%) of the crime rates
#from the histograms of tax rates we concur there are few neighborhoods where they pay more. average tax amount is around $408.20 and median is around $330 
summary(Boston$ptratio)
#from the histograms and the summary we concur that on average there are around 18 students for 1 teacher, and at most there would be 22 students per teacher, and at least there would be 12 students present for one teacher
#q3 D)
rm_over_7 <- subset(Boston, rm>7)
nrow(rm_over_7)
rm("rm_over_7")
#there are 64 susburbs that average more than seven rooms per dwelling.

rm_over_8 <- subset(Boston, rm>8)
nrow(rm_over_8) 
#so there are 13 susburbs that average more than seven rooms per dwelling. 
