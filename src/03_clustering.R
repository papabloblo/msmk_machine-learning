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



km.out <- kmeans(x, 3, nstart = 20)

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

km.out$tot.withinss

tot.withinss <- numeric(10)
for (nclus in 1:10){
  tot.withinss[nclus] <- kmeans(x, nclus, nstart = 20)$tot.withinss
}

tibble(
  nclus = 1:10,
  tot.withinss = tot.withinss
) %>% 
  ggplot(aes(x = nclus, y = tot.withinss)) + 
  geom_line()



# CLUSTER JARÁRQUICO ------------------------------------------------------


hc.complete = hclust(dist(x), method = "complete")
plot(hc.complete)

x2$cluster_jerar <- cutree(hc.complete, 2)

ggplot(data = x2) + 
  geom_point(aes(x = V1, y = V2, color = as.character(cluster_jerar))) +
  labs(
    x = "Cluster"
  )
