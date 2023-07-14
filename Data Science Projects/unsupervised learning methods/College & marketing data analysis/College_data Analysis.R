#Load dependencies
require(ISLR)
require(caret)
require(tidyverse)
library(ggplot2)

#View the data college dataset data frame
data(College) 
head(College)
summary(College)
set.seed(1)

#Creating partitioning
var <- createDataPartition(College$Accept , p = 0.5, list = FALSE)
#pairwise plots ro view correlations
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

#Scaling PhD candidates of the colleges and plotting histogram
var1<-scale(College$PhD, center = TRUE, scale = TRUE)
var2 <- ggplot(College, aes(x=var1))+
  geom_bar()+
  ggtitle("Count of Institutions per phd candidates")+
  labs(x="Accepted",
       y=" Number of Colleges")
var2

#Subsetting the data
head(College$Private)
#Creating Private Data frame
private<-College[College$Private == 'Yes',]
head(private)
save(private, file = "private.Rdata")
#Creating Public Data frame
public<-College[College$Private == 'No',]
head(public)
save(public, file = 'public.Rdata')

#Ordering private in descending order
order(private$Apps, decreasing = TRUE, method = c("auto"))
#ordering in public in decreasing order
order(public$Apps, decreasing = TRUE, method = c("shell"))

#Eliminate all colleges of median less than Top25%
private_elimnated<-College[median(private$Top25perc),]
summary(private)
public_elimnated<-College[median(public$Top25perc),]
summary(public)

#1Convrting to categorical variable graduation rate from values high - low
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

#Creating a list to append both data set
new_list<-list()
#append private data set to list
new_list[[length(new_list)+1]] <- private
#append public data set to list
new_list[[length(new_list)+1]] <- public
new_list
#save it to a .Rdata file
save(new_list, file = "private.Rdata")

