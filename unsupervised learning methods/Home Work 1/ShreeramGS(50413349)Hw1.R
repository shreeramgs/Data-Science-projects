#Q1)
require(ISLR)
require(caret)
require(tidyverse)
library(ggplot2)
data(College) 
head(College)
set.seed(1)
var <- createDataPartition(College$Accept , p = 0.5, list = FALSE)

#Q1.a)
#pairwise plots
pairs(College[var,-var],col="brown")
#histograms: Accepted vs number of college
var1 <- ggplot(College, aes(y=Accept, col="brown" ))+
  geom_bar()+
  ggtitle(" Divide of Accepted Institutions")+
  labs(x="Number of Colleges",
       y="Accepted ")
var1

#histograms of colleges phd candidates 
var1 <- ggplot(College, aes(x=PhD))+
  geom_bar()+
  ggtitle("Count of Institutions per phd candidates")+
  labs(x="PhD",
       y=" Count of Colleges")
var1
#1.b)
#Scaling PhD candidates of the colleges and plotting histogram
var1<-scale(College$PhD, center = TRUE, scale = TRUE)
var2 <- ggplot(College, aes(x=var1))+
  geom_bar()+
  ggtitle("Count of Institutions per phd candidates")+
  labs(x="Accepted",
       y=" Number of Colleges")
var2

#1.c)
head(College$Private)
#Creating Private Data frame
private<-College[College$Private == 'Yes',]
head(private)
save(private, file = "private.Rdata")
#Creating Public Data frame
public<-College[College$Private == 'No',]
head(public)
save(public, file = 'public.Rdata')

#1.d) ordering private in descending order
order(private$Apps, decreasing = TRUE, method = c("auto"))
#ordering in public in decreasing order
order(public$Apps, decreasing = TRUE, method = c("shell"))

#1.e)Eliminate all colleges of median less than Top25%
private_elimnated<-College[median(private$Top25perc),]
summary(private)
public_elimnated<-College[median(public$Top25perc),]
summary(public)

#1.f)graduation rate into high or low
#grad_rate_class of private
grad_rate_class <- ordered(cut(private$Grad.Rate, c(15,40, 70, 100)), labels = c("High", "Medium", "Low") )
summary(grad_rate_class)
#append to private
private$grad_rate_class <-grad_rate_class
head(private)
var1 <- ggplot(private, aes(x=grad_rate_class))+
  geom_bar()+
  ggtitle("Count of Institutions per grad_rate_class candidates")+
  labs(x="grad_rate_class",
       y=" Count of Colleges")
var1
#grad_rate_class of public
grad_rate_class <- ordered(cut(public$Grad.Rate, c(15,40, 70, 100)), labels = c("High", "Medium", "Low") )
summary(grad_rate_class)
#append to public
public$grad_rate_class <-grad_rate_class
head(public)
var1 <- ggplot(public, aes(x=grad_rate_class))+
  geom_bar()+
  ggtitle("Institutions per grad_rate_class candidates")+
  labs(x="grad_rate_class",
       y=" Count of Colleges")
var1

#1.g)Create a list to add both data set

#create a list
new_list<-list()
#append private data set to list
new_list[[length(new_list)+1]] <- private
#append public data set to list
new_list[[length(new_list)+1]] <- public
new_list
#save it to a .Rdata file
save(new_list, file = "private.Rdata")



#Q2)



library(randomForest)
library("rpart") #install.packages("rpart")
#loading the marketing dataset
load("marketing.RData")

dim(marketing) # 8993 x 14
summary(marketing)
#loading the marketing dataset by omitting null values
marketing <- na.omit(marketing)
dim(marketing) # 6876 x 14

randomized_data <- marketing # for question i)
permuted_data <- marketing # for question ii)

for (i in 1:14) {
  min_val = min(marketing[,i])
  max_val = max(marketing[,i])
  randomized_data[,i] = floor(runif(nrow(marketing), min=min_val, max=max_val + 1))
  permuted_data[,i] = sample(min_val: max_val, nrow(marketing), replace = TRUE)
}

marketing$result <- 1
randomized_data$result <- 0
permuted_data$result <- 0

new_data_1 <- rbind(marketing, randomized_data)
new_data_2 <- rbind(marketing, permuted_data)


# i) sample uniformly for each variable
model <- rpart.control(minsplit = 350, xval = 40, cp = 0)
classif <- rpart(result~., data = new_data_1, method = "class", control = model)

x11()
plot(classif, branch = .8, uniform = T, compress = T, main="Classification Tree")

text(classif, use.n = T, all = T, cex = 1)

summary(classif )

x11()
plot(classif$cptable[,4], type = "o", main = "Cp for Model Selection", ylab="cp")

# Pruning
min_cp = which.min(classif$cptable[,5])
pruned_fit <- prune(classif, cp = classif$cptable[min_cp,1])

x11()
plot(pruned_fit, branch = 0.8, compress = T, main = "Pruned Tree")
text(pruned_fit, cex = 0.5)


# ii) by randomly permuting the values within each variable independently.
model.control <- rpart.control(minsplit = 350, xval = 10, cp = 0)
classif <- rpart(result~., data = new_data_2, method = "class", control = model.control)

x11()
plot(classif, branch = .4, uniform = T, compress = T, main="Classification Tree")
text(classif, use.n = T, all = T, cex = 1)

# Pruning
min_cp = which.min(classif$cptable[,4])
pruned_fit <- prune(classif, cp = classif$cptable[min_cp,1])

x11()
plot(pruned_fit, branch = 0.5, compress = T, main = "Pruned Tree")
text(pruned_fit, cex = 0.5)






##
rm(list=ls())
library(arules)
setwd("/Users/Sriram/Desktop/R projects/Sem 2/Hw1/")
library(ISLR2)
head(Boston)
dim(Boston)
?Boston
Boston_data<-Boston
#3a

par(mfrow = c(2,2))
hist(Boston$crim)
hist(Boston$zn)
hist(Boston$indus)
hist(Boston$chas)
hist(Boston$nox)
hist(Boston$rm)
hist(Boston$age)
hist(Boston$dis)
hist(Boston$rad)
hist(Boston$tax)
hist(Boston$ptratio)
hist(Boston$lstat)
hist(Boston$medv)


hist(Boston_data$crim)

boxplot.stats(Boston_data$crim)$stats
Boston_data$crim = ordered(cut(Boston_data[["crim"]],c(10,20,40,60,100)), labels = c('low-Threat','Medium-Threat','High-alert','CodeRed'))


hist(Boston_data$zn)
Boston_data[["zn"]]<- ordered (cut(Boston_data[["zn"]], c (0,15,50,100)), labels= c("no-zone", "less-Zone", "High-Zone"))

hist(Boston_data$indus)
boxplot.stats(Boston_data$indus)$stats
Boston_data$indus = ordered(cut(Boston_data[["indus"]],c(0,10,20,40)), labels = c('Low','Moderate','High'))

hist(Boston_data$chas)
Boston_data[["chas"]]<-ordered(Boston_data$chas,labels=c(" Not bound to river","bound to river"))


hist(Boston_data$nox)
boxplot.stats(Boston_data$nox)$stats
Boston_data$nox = ordered(cut(Boston_data[["nox"]],c(0.1,0.3,0.4,0.8)), labels = c('Low-Concentration','Moderate-Concentration','High-Concentration'))


hist(Boston_data$rm)
boxplot.stats(Boston_data$rm)$stats
Boston_data$rm = ordered(cut(Boston_data[["rm"]],c(0,4,7,9)), labels = c('Small houses','Medium Houses','Big Houses'))


hist(Boston_data$age)
boxplot.stats(Boston_data$age)$stats
Boston_data$age = ordered(cut(Boston_data[["age"]],c(0,25,50,70,100)), labels = c('Silver','Gold','Diamond','Too old'))
Boston_data$age


hist(Boston_data$dis)
boxplot.stats(Boston_data$dis)$stats
Boston_data$dis = ordered(cut(Boston_data[["dis"]],c(0,4,7,14)), labels = c('Near','Far','Very Far'))


hist(Boston_data$rad)
boxplot.stats(Boston_data$rad)$stats
Boston_data$rad = ordered(cut(Boston_data[["rad"]],c(0,2,10,26)), labels = c('Less Accesible','Moderately Accesible','Highly Accesible'))


hist(Boston_data$tax)
boxplot.stats(Boston_data$tax)$stats
Boston_data$tax = ordered(cut(Boston_data[["tax"]],c(187,279,660,711)), labels = c('Low','Moderate','High'))


hist(Boston_data$ptratio)
boxplot.stats(Boston_data$ptratio)$stats
Boston_data$ptratio = ordered(cut(Boston_data[["ptratio"]],c(10,19,26)), labels = c('Low','High'))


hist(Boston_data$lstat)
boxplot.stats(Boston_data$lstat)$stats
Boston_data$lstat = ordered(cut(Boston_data[["lstat"]],c(0,12,40)), labels = c('Low','High'))


hist(Boston_data$medv)
boxplot.stats(Boston_data$medv)$stats
Boston_data$medv = ordered(cut(Boston_data[["medv"]],c(0,22,51)), labels = c('Low','High'))


incedence_matrix = as(Boston_data,"transactions")
incedence_matrix 

#3b

itemFrequencyPlot(incedence_matrix,support=0.001,cex.name= 0.8)

rules<-apriori(incedence_matrix,parameter= list(support = 0.1,confidence = 0.3))
summary(rules)



#3c
rules_medv = subset(rules, subset = lhs %in% "dis=Near" & rhs %in% "medv=Low" & lift>1.2)
inspect(head(sort(rules_medv, by = "confidence"), n = 3))


#3d
rules_ptratio_low<-subset(rules,subset= rhs %in% "ptratio=Low" & lift>1.2)
inspect(head(sort(rules_ptratio_low, by="confidence"),n=3))

