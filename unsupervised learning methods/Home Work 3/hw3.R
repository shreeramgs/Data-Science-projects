#Q10)
#a)
require(tidyverse)
set.seed(1)

ds <- data.frame(replicate(50, rnorm(20, mean = rnorm(1, mean = 0),  sd = 3))) %>%
  rbind(data.frame(replicate(50, rnorm(20, mean = rnorm(1, mean = 1), sd = 3)))) %>%
  rbind(data.frame(replicate(50, rnorm(20, mean = rnorm(1, mean = 2), sd = 3)))) %>%
  as.tibble %>%
  mutate(id = row_number(),
         class = ifelse(id <= 20, 'A',
                        ifelse(id <= 40, 'B',
                               'C'))) %>%
  select(-id)

#b)
require(ggbiplot); require(ggthemes)
pca <- prcomp(ds %>% select(-class), scale = TRUE)
x11()
ggbiplot(pca, groups = ds$class, var.axes = FALSE, 
         ellipse = TRUE) +
  geom_point(aes(col = ds$class), size = 4) +
  theme_tufte(base_size = 16) +
  theme(legend.position = 'top') +
  guides(name = 'Groups') +
  scale_color_discrete(name = 'Class')

#c)
scaling <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
kmeans.clust <- ds %>%
  select(-class) %>%
  kmeans(centers = 3)
x11()
ggbiplot(pca, groups = factor(kmeans.clust$cluster), var.axes = FALSE, 
         ellipse = TRUE) +
  geom_point(aes(shape = ds$class, 
                 col = factor(kmeans.clust$cluster)), size = 4) +
  theme_tufte(base_size = 16) +
  theme(legend.position = 'top') +
  guides(name = 'Groups') +
  theme(legend.position = 'top') +
  scale_color_discrete(name = 'K-Means Groups') +
  scale_shape_discrete(name = 'Real Group')

#d)
kmeans.clust <- ds %>%
  select(-class) %>%
  kmeans(centers = 2)
x11()
ggbiplot(pca, groups = factor(kmeans.clust$cluster), var.axes = FALSE, 
         ellipse = TRUE) +
  geom_point(aes(shape = ds$class, 
                 col = factor(kmeans.clust$cluster)), size = 4) +
  theme_tufte(base_size = 16) +
  theme(legend.position = 'top') +
  guides(name = 'Groups') +
  theme(legend.position = 'top') +
  scale_color_discrete(name = 'K-Means Groups') +
  scale_shape_discrete(name = 'Real Group')
#e)
kmeans.clust <- ds %>%
  select(-class) %>%
  kmeans(centers = 4)
X11()
ggbiplot(pca, groups = factor(kmeans.clust$cluster), var.axes = FALSE, 
         ellipse = TRUE) +
  geom_point(aes(shape = ds$class, 
                 col = factor(kmeans.clust$cluster)), size = 4) +
  theme_tufte(base_size = 16) +
  theme(legend.position = 'top') +
  guides(name = 'Groups') +
  theme(legend.position = 'top') +
  scale_color_discrete(name = 'K-Means Groups') +
  scale_shape_discrete(name = 'Real Group')

#f)
pca_2 <- pca$x %>%
  as.tibble %>%
  select(PC1, PC2)

pca_kmeans <- pca_2 %>%
  kmeans(centers = 3)
X11()
ggplot(pca_2, aes(PC1, PC2, 
                  col = factor(pca_kmeans$cluster), 
                  shape = ds$class)) +
  geom_point(size = 4) +
  theme_tufte(base_size = 14) +
  scale_color_discrete(name = 'K-Means Group') +
  scale_shape_discrete(name = 'Real Class')

#g)
scaling <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

scaled_kmeans <- ds %>%
  select(-class) %>%
  map_df(scaling) %>%
  kmeans(centers = 3)

X11()
ggbiplot(pca, groups = factor(scaled_kmeans$cluster), var.axes = FALSE, 
         ellipse = TRUE) +
  geom_point(aes(shape = ds$class, 
                 col = factor(scaled_kmeans$cluster)), size = 4) +
  theme_tufte(base_size = 16) +
  theme(legend.position = 'top') +
  guides(name = 'Groups') +
  theme(legend.position = 'top') +
  scale_color_discrete(name = 'K-Means Groups') +
  scale_shape_discrete(name = 'Real Group')










######################################################################################
require(ISLR)
require(caret)
require(tidyverse)
library(ggplot2)
library(dendextend)
library("ape")
require(ape)
require(RColorBrewer)
require(corrplot)
setwd("C:/Users/Sriram/Desktop/R projects/Sem 2/HW3")
#a)
#Read the csv file with header = F
dataset_q13 = data <- read.csv("Ch12Ex13.csv",header = F)

#b)
#finding correlation based distance
cdist <- dist(cor(dataset_q13, method = "pearson", use = "everything"))


methods <- c('centroid', 'average', 'single', 'complete')
#Dendograms

for (method in methods) {
  clusts <- hclust(cdist, method = method)
  x11()
  plot(as.phylo(clusts), type = "unrooted",
       cex = 0.6,
       col = "#487AA1", col.main = "#45ADA8", 
       col.lab = "#7C8071", col.axis = "#F38630",
       edge.color = "navyblue",
       edge.width = 2, edge.lty = 2,
       tip.color = "red",
       main = paste0('Cluster Dendrogram using ', method, ' metric'))
}



#cut into two clusters as mainly all the above graphs are cut into 2 clusters

x11()

plot(as.phylo(clusts), type = 'fan', 
     tip.color = brewer.pal(2, 'Set1')[cutree(clusts, 2)],
     edge.color = 'blue', edge.lty = 2,
     cex = 1.5,
     main = 'Dendrogram of cut Clusters')



#COrplot
x11()
corrplot(cor(dataset_q13), method = 'shade',
         diag = FALSE, order = 'hclust',
         rect.col = 'red',
         rect.lwd = 3, tl.pos = 'd',
         tl.col = 'black', tl.cex = 0.7,
         hclust.method = 'centroid',
         col = COL2('RdBu', 200))

#c)
require(ggbiplot)
require(ggthemes)
library(gtExtras)
library(gt)
pca_ds <- prcomp(dataset_q13)

pca_1 <- pca_ds$x %>%
  as.tibble %>%
  select(PC1, PC2)

kmeans_ds <- pca_1 %>%
  kmeans(centers = 2)

x11()
ggbiplot(pca_ds, groups = factor(kmeans_ds$cluster), 
         ellipse = TRUE) +
  theme_tufte(base_size = 14) +
  geom_point(aes(col = factor(kmeans_ds$cluster)), 
             size = 2, alpha = 0.2) +
  theme(legend.position = 'top') +
  scale_color_manual(name = 'K-Means Group of Patient',
                     values = c('#a6cee3', '#e31a1c')) +
  ggtitle('K-Means Clustering of First Two Principal Components')


sep_genes <- dataset_q13 %>%
  dplyr::mutate(Variable = paste0('Gene', row_number())) %>%
  filter(kmeans_ds$cluster == 1) %>%
  select(Variable)
my_tab <- table(sep_genes)
View(my_tab)
nrow(sep_genes)
