
set.seed(2)

x <- matrix(rnorm(50*2), ncol = 2)

x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

plot(x)

km_out2 <- kmeans(x, 2, nstart = 20)

km_out2

km_out2$cluster

plot(x, 
      col = (km_out2$cluster + 1), 
      main = "K-Means Clustering Results with K =2", 
      xlab = "", 
      ylab = "", 
      pch = 20, 
      cex = 2)



km_out3 <- kmeans(x, 3, nstart = 20)

km_out3$cluster

plot(x, 
     col = (km_out3$cluster + 1), 
     main = "K-Means Clustering Results with K = 3", 
     xlab = "", 
     ylab = "", 
     pch = 20, 
     cex = 2)

km_out3$tot.withinss


num_cluster <- 1:20
tot_withinss <- c()

for (k in num_cluster){
  km_out <- kmeans(x, k, nstart = 20)
  tot_withinss <- c(tot_withinss, km_out$tot.withinss)
}

km_results <- data.frame(k = num_cluster,
                         tot_withinss = tot_withinss)

ggplot(km_results, aes(x = k, y = tot_withinss)) + 
  geom_line()
