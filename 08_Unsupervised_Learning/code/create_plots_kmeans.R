library(tidyverse)

iris_data<-iris

iris_data$t_color='red'
iris_data$t_color[which(iris_data$Species=='setosa')]<-'green'
iris_data$t_color[which(iris_data$Species=='virginica')]<-'blue'

iris_data %>%
  ggplot() +
  aes(x=Sepal.Length, y= Sepal.Width, color=Species) +
  geom_point() +
  theme_bw()

k1<-c(7,3)
k2<-c(4.5,4)
k3<-c(6.8,3.2)


initial <- iris_data %>%
  ggplot() +
  aes(x=Sepal.Length, y= Sepal.Width) +
  geom_point() +
  annotate(geom = "point", x = k1[1], y = k1[2], shape = 5, size = 3, color = "#F8766D") +
  annotate(geom = "point", x = k2[1], y = k2[2], shape = 8, size = 3, color = "#7CAE00") +
  annotate(geom = "point", x = k3[1], y = k3[2], shape = 7, size = 3, color = "#00BFC4") +
  theme_bw() +
  scale_x_continuous(limits=c(4,8)) +
  scale_y_continuous(limits=c(2,4.5)) +
  theme(legend.position = "none") 
  

number_of_steps<-10
n<-1

distance <- list()
plots <- list()


iris_data$clust_1 <- NULL
iris_data$clust_2 <- NULL
iris_data$clust_3 <- NULL

for(n in 1:number_of_steps){
  iris_data$distance_to_clust1 <- sqrt((iris_data$Sepal.Length-k1[1])^2+(iris_data$Sepal.Width-k1[2])^2)
  iris_data$distance_to_clust2 <- sqrt((iris_data$Sepal.Length-k2[1])^2+(iris_data$Sepal.Width-k2[2])^2)
  iris_data$distance_to_clust3 <- sqrt((iris_data$Sepal.Length-k3[1])^2+(iris_data$Sepal.Width-k3[2])^2)
  
  iris_data$clust_1 <- 1*(iris_data$distance_to_clust1 <= iris_data$distance_to_clust2 & 
                          iris_data$distance_to_clust1 <= iris_data$distance_to_clust3)
  
  iris_data$clust_2 <- 1*(iris_data$distance_to_clust1 > iris_data$distance_to_clust2 & 
                          iris_data$distance_to_clust3 > iris_data$distance_to_clust2)
  
  iris_data$clust_3 <- 1*(iris_data$distance_to_clust3 < iris_data$distance_to_clust1 & 
                          iris_data$distance_to_clust3 < iris_data$distance_to_clust2)
  
  
  test <- iris_data %>%
    mutate(cluster = case_when(clust_1 == 1 ~ "C1",
                               clust_2 == 1 ~ "C3",
                               clust_3 == 1 ~ "C4"))
  
  plot_dist <- iris_data %>%
    mutate(cluster = case_when(clust_1 == 1 ~ "C1",
                               clust_2 == 1 ~ "C3",
                               clust_3 == 1 ~ "C4")) %>%
    ggplot() +
    aes(x=Sepal.Length, y= Sepal.Width, color = cluster) +
    geom_point() +
    annotate(geom = "point", x = k1[1], y = k1[2], shape = 5, size = 3, color = "#F8766D") +
    annotate(geom = "point", x = k2[1], y = k2[2], shape = 8, size = 3, color = "#7CAE00") +
    annotate(geom = "point", x = k3[1], y = k3[2], shape = 7, size = 3, color = "#00BFC4") +
    theme_bw() +
    scale_x_continuous(limits=c(4,8)) +
    scale_y_continuous(limits=c(2,4.5)) +
    theme(legend.position = "none") 
  
  distance[[n]] <- plot_dist
  
  
  k1[1]<-mean(iris_data$Sepal.Length[which(iris_data$clust_1==1)])
  k1[2]<-mean(iris_data$Sepal.Width[which(iris_data$clust_1==1)])
  k2[1]<-mean(iris_data$Sepal.Length[which(iris_data$clust_2==1)])
  k2[2]<-mean(iris_data$Sepal.Width[which(iris_data$clust_2==1)])
  k3[1]<-mean(iris_data$Sepal.Length[which(iris_data$clust_3==1)])
  k3[2]<-mean(iris_data$Sepal.Width[which(iris_data$clust_3==1)])
 
  
  plot <- iris_data %>%
    mutate(cluster = case_when(clust_1 == 1 ~ "C1",
                               clust_2 == 1 ~ "C2",
                               clust_3 == 1 ~ "C3")) %>%
    ggplot() +
    aes(x=Sepal.Length, y= Sepal.Width, color = cluster) +
    geom_point() +
    annotate(geom = "point", x = k1[1], y = k1[2], shape = 5, size = 3, color = "#F8766D") +
    annotate(geom = "point", x = k2[1], y = k2[2], shape = 8, size = 3, color = "#7CAE00") +
    annotate(geom = "point", x = k3[1], y = k3[2], shape = 7, size = 3, color = "#00BFC4") +

    theme_bw() +
    scale_x_continuous(limits=c(4,8)) +
    scale_y_continuous(limits=c(2,4.5)) +
    theme(legend.position = "none") 
  
  plots[[n]] <- plot
    
}

print(initial)
