#' ANÁLISIS NO SUPERVISADO
#' MSMK: taller de ML
#' 


# K-MEDIAS ----------------------------------------------------------------

set.seed(2)

x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

x2 <- as_tibble(x)

ggplot(data = x2) + 
  geom_point(aes(x = V1, y = V2))

km.out <- kmeans(x, 2, nstart = 20)

km.out

x2$cluster <- km.out$cluster

ggplot(data = x2) + 
  geom_point(aes(x = V1, y = V2, color = as.character(cluster))) +
  geom_point(data = as_tibble(km.out$centers),
             aes(x = V1, V2),
             size = 5) +
  labs(
    x = "Cluster"
  )


#' Conjunto de datos IRIS
#' ¿Cómo seleccionar la k?
?iris


tot.withinss <- numeric(10)
for (nclus in 1:10){
  tot.withinss[nclus] <- kmeans(
    iris[c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")], 
    nclus, 
    nstart = 20)$tot.withinss
}

tibble(
  nclus = 1:10,
  tot.withinss = tot.withinss
) %>% 
  ggplot(aes(x = nclus, y = tot.withinss)) + 
  geom_line() +
  scale_x_continuous(breaks = 1:10)



km.out <- kmeans(iris[c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")], 3, nstart = 20)

km.out$centers

iris$cluster <- km.out$cluster
centroides <- km.out$centers %>% 
  as_tibble() %>% 
  mutate(cluster = 1:3) %>% 
  gather(key = "variable", value = "value", -cluster) 

iris %>% 
  mutate(id = 1:nrow(iris)) %>% 
  gather(key = "variable", value = "value", -Species, -cluster,-id) %>% 
  ggplot(aes(x = variable, y = value, group = id, color = as.character(cluster))) +
  geom_line(alpha = 0.5) +
  geom_line(data = centroides, 
            aes(x = variable, y = value, group = cluster),
            color = "grey2",
            size = 2, 
            linetype = "dashed") +
  labs(title = "Perfilado de clustering",
       color = "Id del cluster") +
  theme_minimal()


# CLUSTER JARÁRQUICO ------------------------------------------------------

hc.complete = hclust(dist(iris[c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")]),
                     method = "complete")
plot(hc.complete)

x2$cluster_jerar <- cutree(hc.complete, 2)

ggplot(data = x2) + 
  geom_point(aes(x = V1, y = V2, color = as.character(cluster_jerar))) +
  labs(
    x = "Cluster"
  )
