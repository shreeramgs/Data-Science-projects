library(randomForest)
library("rpart") #install.packages("rpart")

dim(marketing) # 8993 x 14
summary(marketing)
marketing <- na.omit(marketing)
dim(marketing) # 6876 x 14

randomized_data <- marketing
permuted_data <- marketing

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