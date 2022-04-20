#https://rpubs.com/Joaquin_AR/310338
#probar con archivo aerolineas y traveling
#objetivo traveling divides los usuarios en los lugares turísticos
#que más les atraen ? para dar una recomendación de sitios en base a eso
#objetivo aerolinea conseguir cluster de clientes para ver en qué se diferencian
#sobretodo y aplicar distintas ofertar a cada cluster
library(readr)
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(ISLR)


aerolinea = read_csv("C:/Users/maria/OneDrive/Escritorio/Master/Machine Learning/Practical Application 3/EastWestAirlines.csv",
                     col_types = c("-", rep("n",11)))
travel = read_csv("C:/Users/maria/OneDrive/Escritorio/Master/Machine Learning/Practical Application 3/google_review_ratings.csv",
                  col_types = c("-",rep("n",23),"-"))

euclid_dist <- daisy(travel, metric = "euclidean")
euclid_mat <- as.matrix(euclid_dist)


sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(euclid_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

#create clusters
pam_fit <- pam(euclid_dist, diss = TRUE, k = 2)

pam_results <- travel %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

#summary
pam_results$the_summary

#visualization
tsne_obj <- Rtsne(euclid_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
