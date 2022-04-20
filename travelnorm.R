library(readr)
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(ISLR)
library(factoextra)
library(NbClust)
library(mclust)
library(EMCluster)
library(corrplot)
library(cclust)
library(ggradar)
library(ggpubr)
library(clValid)

travel = read_csv("C:/Users/maria/OneDrive/Escritorio/Master/Machine Learning/Practical Application 3/google_review_ratings.csv",
                  col_types = c("-",rep("n",23),"-"))

#DATA PREPROCESS
#See if there are missing values 
missing = sum(is.na(travel))
#Column name transformation
colnames(travel) = c('churches', 'resorts', 'beaches', 'parks', 'theatres', 
                     'museums', 'malls', 'zoo', 'restaurants', 'pubs_bars', 
                     'local_services', 'burger_pizza_shops', 'hotels_other_lodgings', 
                     'juice_bars', 'art_galleries', 'dance_clubs', 'swimming_pools', 
                     'gyms', 'bakeries', 'beauty_spas', 'cafes', 'view_points',
                     'monuments', 'gardens')
#Low correlation between variables
summary(travel)
corrplot(cor(travel), method = "circle", type = "upper")
#0.62 max correlacion

#Scale the data 
datos = scale(travel)
datos =as.data.frame(datos)


boxplot(datos)

#PCA
pca = prcomp(datos)

#####PARTITIONAL CLUSTERING#####
#K-MEANS
####################ELBOW METHOD######################
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 10,
             diss = dist(datos, method = "manhattan"))
####################SILHOUETTE METHOD######################
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "silhouette", k.max = 10,
             diss = dist(datos, method = "manhattan"))
####################GAP STATISTIC######################
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "gap_stat", k.max = 10,
             diss = dist(datos, method = "manhattan"))
#K = 2
km2_clusters <- kmeans(x = datos, centers = 2, nstart = 50)
km2_clusters
#Cluster visualization
fviz_cluster(object = km2_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "t", star.plot = TRUE, repel = TRUE) +
  labs(title = "Results clustering K-means, K=2") +
  theme_bw() +
  theme(legend.position = "none")

#Radarchart
km2_centers = km2_clusters$centers
df = cbind(cluster =c(1,2), km2_centers)
ggradar(df,grid.min = -2,grid.max = 2)

#K=7
km7_clusters <- kmeans(x = datos, centers = 7, nstart = 50)
km7_clusters
#Cluster visualization
fviz_cluster(object = km7_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "t", star.plot = TRUE, repel = TRUE) +
  labs(title = "Results clustering K-means, K=7") +
  theme_bw() +
  theme(legend.position = "none")

#Radarchart
km7_centers = km7_clusters$centers
df = cbind(cluster =c(1,2,3,4,5,6,7), km7_centers)
ggradar(df,grid.min = -3,grid.max = 4)

#PAM
####################ELBOW METHOD######################
fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 10,
             diss = dist(datos, method = "manhattan"))
####################SILHOUETTE METHOD######################
fviz_nbclust(x = datos, FUNcluster = pam, method = "silhouette", k.max = 10,
             diss = dist(datos, method = "manhattan"))
####################GAP STATISTIC######################
fviz_nbclust(x = datos, FUNcluster = pam, method = "gap_stat", k.max = 10,
             diss = dist(datos, method = "manhattan"))

#K=2
pam2_clusters <- pam(x = datos, k = 2, nstart = 50, metric = "manhattan")
pam2_clusters
#Cluster visualization
fviz_cluster(object = pam2_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "t", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering PAM, K=2") +
  theme_bw() +
  theme(legend.position = "none")

#Radarchart
pam2_centers = pam2_clusters$medoids
df = cbind(cluster =c(1,2), pam2_centers)
ggradar(df,grid.min = -2,grid.max = 2)

#K=7
pam7_clusters <- pam(x = datos, k = 7, nstart = 50, metric = "manhattan")
pam7_clusters
#Cluster visualization
fviz_cluster(object = pam7_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "t", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering PAM, K=7") +
  theme_bw() +
  theme(legend.position = "none")

#Radarchart
pam7_centers = pam7_clusters$medoids
df = cbind(cluster =c(1,2,3,4,5,6,7), pam7_centers)
ggradar(df,grid.min = -3,grid.max = 3)

#CLARA
####################ELBOW METHOD######################
fviz_nbclust(x = datos, FUNcluster = clara, method = "wss", k.max = 10,
             diss = dist(datos, method = "manhattan"))
####################SILHOUETTE METHOD######################
fviz_nbclust(x = datos, FUNcluster = clara, method = "silhouette", k.max = 10,
             diss = dist(datos, method = "manhattan"))
####################GAP STATISTIC######################
fviz_nbclust(x = datos, FUNcluster = clara, method = "gap_stat", k.max = 10,
             diss = dist(datos, method = "manhattan"))

#K=2
clara2_clusters <- clara(x = datos, k = 2, metric = "manhattan", stand = TRUE,
                        samples = 50, pamLike = TRUE)
clara2_clusters
#Cluster visualization
fviz_cluster(object = clara2_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "t", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering CLARA, K=2") +
  theme_bw() +
  theme(legend.position = "none")
#Radarchart
clara2_centers = clara2_clusters$medoids
df = cbind(cluster =c(1,2), clara2_centers)
ggradar(df,grid.min = -2,grid.max = 2)

#K=7
clara7_clusters <- clara(x = datos, k = 7, metric = "manhattan", stand = TRUE,
                        samples = 50, pamLike = TRUE)
clara7_clusters
#Cluster visualization
fviz_cluster(object = clara7_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "t", star.plot = TRUE, repel = TRUE) +
  labs(title = "Results clustering CLARA, K=7") +
  theme_bw() +
  theme(legend.position = "none")

#Radarchart
clara7_centers = clara7_clusters$medoids
df = cbind(cluster =c(1,2,3,4,5,6,7), clara7_centers)
ggradar(df,grid.min = -4,grid.max = 4)

#HIERARCHICAL
######################AGGLOMERATIVE#############################
dist <- daisy(datos, metric = "manhattan")
mat <- as.matrix(dist)

hc_manhattan_completo <- hclust(d = dist, method = "complete")
hc_manhattan_single   <- hclust(d = dist, method = "single")
hc_manhattan_average  <- hclust(d = dist, method = "average")
hc_manhattan_centroid  <- hclust(d = dist, method = "centroid")
hc_manhattan_ward  <- hclust(d = dist, method = "ward.D")
cor = c()
cor[1] = cor(x = dist, cophenetic(hc_manhattan_completo))
cor[2] = cor(x = dist, cophenetic(hc_manhattan_single))
cor[3] = cor(x = dist, cophenetic(hc_manhattan_average))
cor[4] = cor(x = dist, cophenetic(hc_manhattan_centroid))
cor[5] = cor(x = dist, cophenetic(hc_manhattan_ward))

barplot(cor, 
        names.arg = c("complete","single","average","centroid","ward"),
        col = terrain.colors(5)) 

dend_1 = as.dendrogram(hc_manhattan_average)

fviz_dend(x = hc_manhattan_average, cex = 0.6)+
  labs(title = "Hierarchical clustering",
       subtitle = "Manhattan distance, Linkage average")

clusters_agglo <- cutree(tree = hc_manhattan_average, k = 2)


######################DIVISIVE#############################
hc_diana <- diana(x = dist, diss = TRUE, stand = FALSE)

dend_2 = as.dendrogram(hc_diana)

fviz_dend(x = hc_diana, cex = 0.5) +
  labs(title = "Divisive Hierarchical clustering",
       subtitle = "Manhattan distance")

clusters_diana <- cutree(tree = hc_diana, k = 2)
tab_diana = table(clusters_diana, dnn = list("clusters", "tipo de cliente"))

#Comparing dendograms
tanglegram(dend1 = dend_1, dend2 = dend_2, highlight_distinct_edges = TRUE,
           common_subtrees_color_branches = TRUE)
cor_cophenetic(dend1 = dend_1, dend2 = dend_2)

#EM
set.seed(1)
model_clustering1 <- Mclust(data = datos, G = 1:10)
summary(model_clustering1)
summary(model_clustering1$BIC)

fviz_mclust(object = model_clustering1, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

model_clustering2 <- Mclust(data = datos, G = 10, modelNames = "EEV")
model_clustering3 <- Mclust(data = datos, G = 8, modelNames = "EEV")

fviz_mclust(model_clustering1, what = "uncertainty", pallete = "jco")+
  labs(title = "EM (Model VEV)",
       subtitle = "Manhattan distance,K=10, ")
fviz_mclust(model_clustering2, what = "uncertainty", pallete = "jco")+
  labs(title = "EM (Model EEV)",
       subtitle = "Manhattan distance,K=10, ")
fviz_mclust(model_clustering3, what = "uncertainty", pallete = "jco")+
  labs(title = "EM (Model EEV)",
       subtitle = "Manhattan distance,K=8, ")

#Radarchart 1
clust_model1 = list()
for (i in 1:10) {
  clust_model1[[i]] = datos[model_clustering1$classification==i,]
}

mat = matrix(0,10,24)
for (i in 1:10){
  for (j in 1:24){
    mat[i,j] = mean(clust_model1[[i]][,j])
  }
}


df = cbind(cluster =c(1,2,3,4,5,6,7,8,9,10), mat)
colnames(df) = c('cluster','churches', 'resorts', 'beaches', 'parks', 'theatres', 
                     'museums', 'malls', 'zoo', 'restaurants', 'pubs_bars', 
                     'local_services', 'burger_pizza_shops', 'hotels_other_lodgings', 
                     'juice_bars', 'art_galleries', 'dance_clubs', 'swimming_pools', 
                     'gyms', 'bakeries', 'beauty_spas', 'cafes', 'view_points',
                     'monuments', 'gardens')
ggradar(df,grid.min = -3,grid.max = 3)

#Radarchart 2
clust_model2 = list()
for (i in 1:10) {
  clust_model2[[i]] = datos[model_clustering2$classification==i,]
}

mat2 = matrix(0,10,24)
for (i in 1:10){
  for (j in 1:24){
    mat2[i,j] = mean(clust_model2[[i]][,j])
  }
}


df2 = cbind(cluster =c(1,2,3,4,5,6,7,8,9,10), mat2)
colnames(df2) = c('cluster','churches', 'resorts', 'beaches', 'parks', 'theatres', 
                 'museums', 'malls', 'zoo', 'restaurants', 'pubs_bars', 
                 'local_services', 'burger_pizza_shops', 'hotels_other_lodgings', 
                 'juice_bars', 'art_galleries', 'dance_clubs', 'swimming_pools', 
                 'gyms', 'bakeries', 'beauty_spas', 'cafes', 'view_points',
                 'monuments', 'gardens')
ggradar(df2,grid.min = -3,grid.max = 3)

#Radarchart 3
clust_model3 = list()
for (i in 1:8) {
  clust_model3[[i]] = datos[model_clustering3$classification==i,]
}

mat3 = matrix(0,8,24)
for (i in 1:8){
  for (j in 1:24){
    mat3[i,j] = mean(clust_model3[[i]][,j])
  }
}


df3 = cbind(cluster =c(1,2,3,4,5,6,7,8), mat3)
colnames(df3) = c('cluster','churches', 'resorts', 'beaches', 'parks', 'theatres', 
                 'museums', 'malls', 'zoo', 'restaurants', 'pubs_bars', 
                 'local_services', 'burger_pizza_shops', 'hotels_other_lodgings', 
                 'juice_bars', 'art_galleries', 'dance_clubs', 'swimming_pools', 
                 'gyms', 'bakeries', 'beauty_spas', 'cafes', 'view_points',
                 'monuments', 'gardens')
ggradar(df3,grid.min = -3,grid.max = 3)

#VALIDATION
intern = clValid(datos, nClust = c(2,7),
                  clMethods = c("kmeans", "pam", "clara","model"),
                  validation = "internal", metric = "manhattan",
                  method = "average", maxitems = nrow(datos))
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(intern, legend=FALSE)
plot(NULL)
legend("center", clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

stab = clValid(datos, nClust = c(2,7),
                 clMethods = c("kmeans", "pam", "clara","model"),
                 validation = "stability", metric = "manhattan",
                 method = "average", maxitems = nrow(datos))
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(stab, measure=c("APN","AD","ADM"),legend=FALSE)
plot(NULL)
legend("center", clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))
par(op)