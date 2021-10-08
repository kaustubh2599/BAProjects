
##install necessary packages
install.packages("tidyverse")
install.packages("gt")
install.packages("cluster")
#load packages
library(readxl)
library(tidyverse)
library(gt)
library(cluster)
#importing data from Excel
ca <- read.csv("C:\\Users\\LENOVO\\Documents\\SmartWatch.csv")
#explore data set
names(ca)
summary(ca)

##Part 1
df <- ca
names(df)
##Segmentation step
#standardize values
dfz <- scale(df)
## Ward Hierarchical Clustering
# calculate distance matrix with euclidian distance
dis <- dist(dfz, method = "euclidean")
#clustering algorithm
fit <- hclust(dis, method="ward.D2")
# display dendrogram
plot(fit)
# cut dendrogram into 4 clusters
cluster <- cutree(fit, k=4)
cluster
table(cluster)
# draw dendogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border="red")
#add cluster to original data set
df_final <- cbind(df, cluster)
names(df_final)
View(df_final)

##Part 2
##Description step
#calculate segment size in percentages
proportions <- table(df_final$cluster)/length(df_final$cluster)
percentages <- proportions*100
percentages


#Explore mean values of variables in clusters
segments <- df_final %>%
  group_by(cluster) %>%
  summarise_at(vars(TimelyInf, TaskMgm,   DeviceSt, Wellness, Athlete, Style, AmznP,Female, Degree, Income, Age, cluster),
            
               list(M = mean))
segments
#Create simple table with Mean values
segments %>%
  gt() %>%
  tab_header(
    title = md("Cluster's Mean Values
               "))
##Part1 and Part2 Cna run separately to get Dendogram and Mean Value Table 


