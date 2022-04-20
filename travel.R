#https://rpubs.com/Joaquin_AR/310338

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

travel = read_csv("C:/Users/maria/OneDrive/Escritorio/Master/Machine Learning/Practical Application 3/google_review_ratings.csv",
                  col_types = c("-",rep("n",23),"-"))

km2_clusters <- cclust(x = travel, centers = 2, 
                       dist="manhattan", method = "kmeans")
km2_clusters
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

#Boxplot for outliers (arreglar)
ggplot(travel, aes(x=as.factor(colnames(travel)),y=select(churches:gardens))) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")

boxplot(datos)


#NORMALIZAR???
#K-MEANS
####################ELBOW METHOD######################
fviz_nbclust(travel, kmeans, method = "wss")
####################SOLHOUETTE METHOD######################
fviz_nbclust(travel, kmeans, method = "silhouette")
####################GAP STATISTIC######################
fviz_nbclust(travel,kmeans, method = "gap_stat")

km_clusters <- kmeans(x = travel, centers = 7, nstart = 50)
km_clusters
#Cluster visualization
fviz_cluster(object = km_clusters, data = travel, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")


#HIERARCHICAL
######################AGGLOMERATIVE#############################
dist <- daisy(travel, metric = "manhattan")
mat <- as.matrix(dist)

hc_euclidea_completo <- hclust(d = dist, method = "complete")
hc_euclidea_single   <- hclust(d = dist, method = "single")
hc_euclidea_average  <- hclust(d = dist, method = "average")
hc_euclidea_centroid  <- hclust(d = dist, method = "centroid")
hc_euclidea_ward  <- hclust(d = dist, method = "ward.D")
cor = c()
cor[1] = cor(x = dist, cophenetic(hc_euclidea_completo))
cor[2] = cor(x = dist, cophenetic(hc_euclidea_single))
cor[3] = cor(x = dist, cophenetic(hc_euclidea_average))
cor[4] = cor(x = dist, cophenetic(hc_euclidea_centroid))
cor[5] = cor(x = dist, cophenetic(hc_euclidea_ward))

barplot(cor, xlab = c("complete","single","average","centroid","ward")) #usar ggplot para ponerlo bonito

fviz_dend(x = hc_euclidea_average, k = 6, cex = 0.6)+
  geom_hline(yintercept = 30, linetype = "dashed") +
  labs(title = "Hierarchical clustering",
       subtitle = "Manhattan distance, Linkage average, K=6")
fviz_cluster(object = list(data=travel, cluster=cutree(hc_euclidea_average, k=6)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE)  +
  labs(title = "Hierarchical clustering + Proyección PCA",
       subtitle = "Distancia euclídea, Lincage complete, K=4") +
  theme_bw() +
  theme(legend.position = "bottom")

clusters <- cutree(tree = hc_euclidea_completo, k = 6)
table(clusters, dnn = list("clusters", "tipo de cliente"))

#EM
model_clustering <- Mclust(data = travel, G = 1:10)
summary(model_clustering)
summary(model_clustering$BIC)

fviz_mclust(object = model_clustering, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

fviz_mclust(model_clustering, what = "classification", geom = "point",
            pallete = "jco")

fviz_mclust(model_clustering, what = "uncertainty", pallete = "jco")





plot(x = hc_euclidea_average, cex = 0.6, sub = "",
     main = "Manhattan distance, Linkage average, k=3",
     xlab = "", ylab = "")
abline(h = 1000000, lty = 2)

avg_dend_obj <- as.dendrogram(hc_euclidea_completo)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

#euclid_dist <- daisy(travel, metric = "euclidean")
#euclid_mat <- as.matrix(euclid_dist)


#create clusters
#pam_fit <- pam(euclid_dist, diss = TRUE, k = 2)

#pam_results <- travel %>%
#  mutate(cluster = pam_fit$clustering) %>%
#  group_by(cluster) %>%
#  do(the_summary = summary(.))

#summary
#pam_results$the_summary

#visualization
tsne_obj <- Rtsne(euclid_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#KMEANS
set.seed(101)


datos <- travel %>% mutate(cluster = km_clusters$cluster)
datos <- datos %>% mutate(cluster = as.factor(cluster))

#ggplot(data = datos, aes(x = x, y = y, color = as.factor(cluster))) +
 # geom_point(size = 3) +
  #labs(title = "Kmenas con k=2") +
  #theme_bw() +
  #theme(legend.position = "none")
