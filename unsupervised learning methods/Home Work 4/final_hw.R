##################################################
#Q1)
library(ggm)
library(gRbase)
library(bnlearn)
library(gRain)
library(dagitty)
data(cad1)
summary(cad1)
head(cad1)

#To infer knowledge regarding the dataset.
help(cad1)
##################################################
#a)

#Create a smaller dataset on which we shall perform structural learning 
df <- subset(cad1, select = c(Sex,Hyperchol,SuffHeartF, Smoker,Heartfail,CAD))
#use grow Shrink algorithm constrained based Structure learning algorithm
bn_gs_Clrn <- gs(df)
bn_gs_Clrn
#hierarchical clustering Score-based methods
bn_hc_Sclrn <- hc(df, score = "aic")
bn_hc_Sclrn

#plotting graphs of the above 2 netwroks side by side and compare them
x11()
graphviz.plot(bn_gs_Clrn)
x11()
graphviz.plot(bn_hc_Sclrn,shape = "ellipse", highlight = list(arcs = c("CAD","Heartfail")))

#Now we shall compare both of our networks
compare(bn_gs_Clrn, bn_hc_Sclrn)
##################################################
#b)

#finding out the conditional probability tables (CPTs) at each node
fittedbn <- bn.fit(bn_hc_Sclrn, data = df)
fittedbn
cad1_dag <- dag(bn_hc_Sclrn)
#################################################
#Creating a Directed Acyclic Graph
df_dag <- dag(c("CAD","Hyperchol"),c("SuffHeartF","Hyperchol"),c("Heartfail","Hyperchol"),c("Sex","CAD"),c("Smoker","CAD"), c("Heartfail","Smoker"),c("SuffHeartF", "Heartfail") )

#Identifying d-separations in the DAG
dSep(as(df_dag, "matrix"),first = "Sex", second="SuffHeartF", cond = "Hyperchol")
dSep(as(cad1_dag, "matrix"),first = "Smoker", second="CAD", cond = "Heartfail")
dSep(as(cad1_dag, "matrix"),first = "Sex", second="SuffHeartF", cond = "CAD")

#Create CPT 
cpt <- extractCPT(df, df_dag, smooth = 0.5)
cpt
#################################################
#c)
## Build the network
#creating conditional probability tables
ctable <- compileCPT(cpt)
ctable
grn1 <- grain(ctable)
querygrain(grn1, nodes=c("CAD", "Heartfail"), type="marginal")
summary(grn1)
#likelihood evidence
evd <- setFinding(grn1, flist=list(c("Sex", "Female"), c("Hyperchol", "yes")))
getFinding(evd)
querygrain(evd, nodes=c("CAD", "Heartfail"), type="marginal")
#d)Creating dataset
new_df <- simulate(evd, n = 100 , seed = NULL)
summary(new_df)
head(new_df)
typeof(new_df)

#new data in a table
table(new_df)

#finding probability of heart-failure and coronary artery disease
pred <- predict(grn1, response = c("Smoker","CAD"), newdata= new_df,se=TRUE, vcov.=hccm)
pred
bn_newdf <- hc(new_df, score = "aic")
fbn_newdf <- bn.fit(bn_newdf, data = df)
fbn_newdf
##################################################
##################################################


#Q2)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
titanic <- read.csv(file = "titanic.csv", header = TRUE, sep = ",")
summary(titanic)

#representing data set in a table format
tbl_df(titanic)
head(titanic)

#total number of passengers
totalpassengers <- tally(titanic)
totalpassengers

#factoring into child and adult
ind = which(is.na(titanic$Age))
titanic[["Age"]]=replace(titanic$Age,c(ind),28)
titanic[["Age"]]=ifelse(titanic$Age<18,"Child","Adult")
titanic$Age=factor(titanic$Age,levels = c("Child","Adult"))

#viewing the changed result
head(titanic)



##################################################
#Analysis of Survived

#Survived passeneger subset
survivors <-  subset(titanic, Survived == 1)
head(survivors)
#dead passenger subset
dead <-  subset(titanic, Survived == 0)
head(dead)
total_survivors<- tally(survivors)
surv_perc <- round((total_survivors/totalpassengers)*100)
surv_perc
pie(table(titanic$Survived),main = "PIE CHART FOR SURVIVAL RATE with 0 representing dead and 1 alive",
    col = c('red','green'),cex=0.6)
##################################################

#Analysis of  women and children surviving
women_child_survived <- subset(survivors,  Sex == "female" & Age == "Child")
head(women_child_survived)
tot_WCS <- tally(women_child_survived)
WCS_perc <- round((tot_WCS/totalpassengers)*100)
WCS_perc
##################################################
#distribution of Sex with respect to class
ggplot(titanic,aes(x=factor(Pclass),fill=factor(Sex)))+
  geom_bar(position="dodge")
#Graph of mother and child surviving with respect to class
ggplot(women_child_survived,aes(x=factor(Pclass),fill=factor(Parents.Children.Aboard)))+
  geom_bar(position="dodge")
##################################################
tree_1<-rpart(Survived ~ Pclass, data = titanic, method = "class",cp = .02)
rpart.plot(tree_1,main = "titanic survived with respect to class")
##################################################
tree_2 <- rpart(Survived ~ Pclass + Sex + Age + Fare, data = titanic, method = "class",cp = .02)
x11()
rpart.plot(tree_2,main = " characteristics/demographics are more likely in passengers that perished ")
##################################################

#Probability of Rose and Jack Surviving
tree_3 <- rpart(Survived ~ Pclass + Age , data = titanic, method = "class",cp = .02)
rpart.plot(tree_3,main = "titanic survived with respect to class")
############################################################################
############################################################################
#Q4
library(gRbase) #CRA
library(gRain) #CRAN
library(glasso) #CRAN
library(graph) #Bioconductor
library(corrplot)
data(state)
?state

df <- state.x77
head(df)

names(df)

df=(df)


M <- cor(df)
x11()
corrplot(M)


fit.pca <- prcomp(scale(df))
xlim_1 <- min(fit.pca$x[,1])-1
xlim_2 <- max(fit.pca$x[,1])+1
ylim_1 <- min(fit.pca$x[,2])-1
ylim_2 <- max(fit.pca$x[,2])+1

head(df)



x11()
biplot(fit.pca, choices = c(1,2), scale = 0, xlim = c(xlim_1, xlim_2), ylim = c(ylim_1, ylim_2))

S.body <- cov.wt(df, method = "ML")
PC.body <- cov2pcor(S.body$cov)
diag(PC.body) <- 0

x11()
heatmap(PC.body)



ls("package:glasso")

# Estimate a single graph
S <- S.body$cov
m0.lasso <- glasso(S, rho = 100) 
names(m0.lasso)
my.edges <- m0.lasso$wi != 0 
diag(my.edges) <- 0 
g.lasso <- as(my.edges, "graphNEL") 
nodes(g.lasso) <- names(data.frame(state_data))

x11()
plot(g.lasso)





library(geneplotter)
graphics.off()
my_rhos <- c(2,5,10,15,25,50)
m0.lasso <- glassopath(S, rho = my_rhos)
for (i in 1:length(my_rhos)){
  my.edges <- m0.lasso$wi[ , , i] != 0 
  diag(my.edges) <- 0
  g.lasso <- as(my.edges, "graphNEL") 
  nodes(g.lasso) <- names(data.frame(df))
  
  x11()
  plot(g.lasso)
}



data(state)
head(state.x77)

df= state.x77
df=scale(state.x77)


library(kohonen)

set.seed(100)
som_grid = somgrid(xdim = 5, ydim=5, topo = "hexagonal")
df.som = som(df,grid = som_grid,rlen = 10000)

x11()
plot(df.som)

codes = df.som$codes[[1]]

x11()
plot(df.som,type = "changes")

x11()
plot(df.som,type = "count")


x11()
plot(df.som,type = "mapping")


x11()
plot(df.som,type = "dist.neighbours")

d = dist(codes)
hq = hclust(d)

x11()
plot(hq)



smc = cutree(hq,k = 4)


my_pal = c("red","blue","green","pink")
my_bhcol = my_pal[smc]

x11()
plot(df.som,type="mapping",col = "black",bgcol = my_bhcol)
add.cluster.boundaries(df.som,smc)
