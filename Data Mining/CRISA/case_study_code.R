
setwd('C:/Users/rakesh/Desktop/Business  data mining/Assignment 6')

library(readxl)

## reading data 
soap <- read_excel("BathSoap_Data.xls",sheet ='DM_Sheet')

# picking up the required columns only for clustering
data <- soap[,-c(2:10)]


###### 1 a) Clustering based on purchase behaviour ########

# shortlisting the variables 
data_purchase <- data[,c(3:7,10,14:22)]


# checking for any missing values and omitting them 
colSums(is.na(data_purchase))
data_purchase1 <- na.omit(data_purchase)

# Defininig maximum brand loyalty and share of transaction towards other brands
data_purchase1$max_brand_loyalty <- apply(data_purchase1[,c(7:14)],1,max)
data_purchase1 <- data_purchase1[,-c(7:14)]

normalize <- function(x){(x-min(x))/(max(x)-min(x))}


data_purchase1[,1:6] <- sapply(data_purchase1[,1:6],normalize)
summary(data_purchase1)


plot(data_purchase1, main = "initial plot", pch =20, cex =2)

# finding the optimal number of clusters for Kmeanskmeans
remove(wss)

wss <- (nrow(data_purchase1)-1)*sum(apply(data_purchase1,2,var))
for (i in 1:10) {wss[i] <- sum(kmeans(data_purchase1, centers=i)$withinss)}
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
      main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)

# ideal k would be 8, but for pratical purposes of promotion, we limit  k to 5
set.seed(1234)

km_purchase_3 <- kmeans(data_purchase1,3,nstart = 100) 
km_purchase_4 <- kmeans(data_purchase1,4,nstart = 100) 
km_purchase_5 <- kmeans(data_purchase1,5,nstart = 100) 


### results of kmeans
km_purchase_3$withinss
km_purchase_3$tot.withinss
km_purchase_3$betweenss
km_purchase_3$size
km_purchase_3$totss

km_purchase_4$withinss
km_purchase_4$tot.withinss
km_purchase_4$betweenss
km_purchase_4$size
km_purchase_4$totss

km_purchase_5$withinss
km_purchase_5$tot.withinss
km_purchase_5$betweenss
km_purchase_5$size
km_purchase_5$totss


# plotting the results
plot(data_purchase1, col = km_purchase$cluster , 
     main="K-Means result with 5 clusters", pch=20, cex=2)
points(km_purchase$centers, col=col, pch=19, cex=2)


# install.packages('cluster')
# install.packages('fpc')

library(cluster)
library(fpc)

clusplot(data_purchase1, km_purchase_3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
clusplot(data_purchase1, km_purchase_4$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
clusplot(data_purchase1, km_purchase_5$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

##### plotting the centroids 
library(reshape2)
library(ggplot2)

# For k=3
km_center <- as.data.frame(km_purchase_3$centers)
km_center <- cbind(km_center,c(1:3))
names(km_center)[9] <- "cluster"
km_center$cluster <- as.factor(km_center$cluster)
km_center <- km_center[,c(9,1:8)]
km_center_melted <- melt(km_center,id.vars='cluster')
ggplot(km_center_melted, aes(x = variable, y = value)) + geom_line(aes(color = cluster, group = cluster))

# For k=4
km_center <- as.data.frame(km_purchase_4$centers)
km_center <- cbind(km_center,c(1:4))
names(km_center)[9] <- "cluster"
km_center$cluster <- as.factor(km_center$cluster)
km_center <- km_center[,c(9,1:8)]
km_center_melted <- melt(km_center,id.vars='cluster')
ggplot(km_center_melted, aes(x = variable, y = value)) + geom_line(aes(color = cluster, group = cluster))

# For k=5
km_center <- as.data.frame(km_purchase_5$centers)
km_center <- cbind(km_center,c(1:5))
names(km_center)[9] <- "cluster"
km_center$cluster <- as.factor(km_center$cluster)
km_center <- km_center[,c(9,1:8)]
km_center_melted <- melt(km_center,id.vars='cluster')
ggplot(km_center_melted, aes(x = variable, y = value)) + geom_line(aes(color = cluster, group = cluster))


km_purchase_4$cluster

data_demo <- cbind(na.omit(soap[,c(1:10)]),km_purchase_5$cluster,data_purchase1$max_brand_loyalty,data_basis[,c(1:11)])
names(data_demo)[11] <- "cluster"
names(data_demo)[12] <- "loyalty"
write.csv(data_demo,"data_demo2.csv")


library(dplyr)


data_price_summary <- data_demo %>%
                      group_by(cluster) %>%
                      summarise(Cat1=mean(data_demo$`Pr Cat 1`),
                                Cat2=mean(data_demo$`Pr Cat 2`),
                                Cat3=mean(data_demo$`Pr Cat 3`),
                                Cat4=mean(data_demo$`Pr Cat 4`))


counts <- table(data_demo$loyalty,data_demo$cluster)
barplot(counts, main="Socio economic distribution",
        xlab="clusters", col=c("darkblue","red","green","darkred"),
        legend = rownames(counts),beside=TRUE)


counts_food<-table(data_demo$FEH,data_demo$cluster)
barplot(counts_food, main="Food habits distribution",
        xlab="clusters", col=c("darkblue","red","green","darkred"),
        legend = rownames(counts_food),beside=TRUE)


counts_gender<-table(data_demo$SEX,data_demo$cluster)
barplot(counts_gender, main="Gender distribution",
        xlab="clusters", col=c("yellow","red","green"),
        legend = rownames(counts_gender),beside=TRUE)

counts_age<-table(data_demo$AGE,data_demo$cluster)
barplot(counts_age, main="Age distribution",
        xlab="clusters", col=c("darkblue","red","green","darkred"),
        legend = rownames(counts_age),beside=TRUE)


counts_child<-table(data_demo$CHILD,data_demo$cluster)
barplot(counts_child, main="Children distribution",
        xlab="clusters", col=c("darkblue","red","green","darkred","darkgreen"),
        legend = rownames(counts_child),args.legend = list(x = "topright", bty = "n",inset=c(0,-0.2)),beside=TRUE)

counts_edu<-table(data_demo$EDU,data_demo$cluster)
barplot(counts_edu, main="Education distribution",
        xlab="clusters", col=c("darkblue","red","green",
                               "darkred","darkgreen","yellow",
                               "lightblue","brown","darkorange","darksalmon"),
        legend = rownames(counts_edu),args.legend = list(x = "topright", bty = "n",inset=c(0,-0.2),cex=0.5),beside=TRUE)


counts_CS<-table(data_demo$CS,data_demo$cluster)
barplot(counts_CS, main="CS distribution",
        xlab="clusters", col=c("darkblue","red","green"),
        legend = rownames(counts_CS),args.legend = list(x = "topright", bty = "n",inset=c(0,-0.2),cex=0.5),beside=TRUE)





###### 1 b) Clustering based on basis of purchase ########

data_basis <- data[,c(11:13,23:37)]
summary(data_basis)

## no need for normalization 

data_basis <- na.omit(data_basis)


## exploring the selling proposition data
prop_check <- list()
for (i in 8:18)
{
prop_check[i] <- sum(data_basis[,i] == 0)/nrow(data_basis)
}

prop_check <- as.data.frame.matrix(prop_check)


# more than 50% of values have 0  value for columns seliing propositions prop_cat_9 to prop_cat_15 

#hence only prop_cat_5 to prop_cat_8 is considered 
data_basis <- data_basis[,c(1:11)]

## optimal K
wss_1 <- (nrow(data_basis)-1)*sum(apply(data_basis,2,var))
for (i in 1:10) {wss_1[i] <- sum(kmeans(data_basis, centers=i)$withinss)}
plot(1:10, wss_1, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)

# ideal k would be 7, but for pratical purposes of promotion, we limit  k to 5
set.seed(6)
km_basis <- kmeans(data_basis,5,nstart = 100) 
km_basis_4 <- kmeans(data_basis,4,nstart = 100) 


km_all$withinss
km_all$tot.withinss
km_all$betweenss
km_all$size
km_basis$totss

km_basis_4$withinss
km_basis_4$tot.withinss
km_basis_4$betweenss
km_basis_4$size
km_basis_4$totss




# plotting the results
plot(data_basis, col = km_basis$cluster , 
     main="K-Means result with 5 clusters", pch=20, cex=2)
points(km_purchase$centers, col=col, pch=19, cex=2)


library(cluster)
library(fpc)

##cluster PCA plot
clusplot(data_basis, km_basis$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

##### plotting the centroids of each cluster
km_basis_center <- as.data.frame(km_basis$centers)
km_basis_center <- cbind(km_basis_center,c(1:5))
names(km_basis_center)[12] <- "cluster"
km_basis_center$cluster <- as.factor(km_basis_center$cluster)
km_basis_center <- km_basis_center[,c(12,1:11)]
km_basis_center_melted <- melt(km_basis_center,id.vars='cluster')

# install.packages("reshape2")
library(reshape2)
library(ggplot2)
ggplot(km_basis_center_melted, aes(x = variable, y = value)) + geom_line(aes(color = cluster, group = cluster))























###### 1 c) Clustering based on basis of both########
data_all <- cbind(data_purchase1,data_basis)

# optimal k
wss_2 <- (nrow(data_all)-1)*sum(apply(data_all,2,var))
for (i in 1:10) {wss_2[i] <- sum(kmeans(data_all, centers=i)$withinss)}
plot(1:10, wss_2, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)

# ideal k would be 7, but for pratical purposes of promotion, we limit  k to 5
set.seed(9)
km_all <- kmeans(data_all,5,nstart = 100) 
km_all_4 <- kmeans(data_all,4,nstart = 100) 


  km_all$withinss
  km_all$tot.withinss
  km_all$betweenss
  km_all$size
  km_all$totss
  
  km_all_4$withinss
  km_all_4$tot.withinss
  km_all_4$betweenss
  km_all_4$size
  km_all_4$totss
  
  
   

# plotting the results
plot(data_all, col = km_all$cluster , 
     main="K-Means result with 5 clusters", pch=20, cex=2)
points(km_all$centers, col=col, pch=19, cex=2)


library(cluster)
library(fpc)

##cluster PCA plot
clusplot(data_all, km_all$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

##### plotting the centroids of each cluster
km_all_center <- as.data.frame(km_all$centers)
km_all_center <- cbind(km_all_center,c(1:5))
names(km_all_center)[20] <- "cluster"
km_all_center$cluster <- as.factor(km_all_center$cluster)
km_all_center <- km_all_center[,c(20,1:19)]
km_all_center_melted <- melt(km_all_center,id.vars='cluster')

# install.packages("reshape2")
library(reshape2)
library(ggplot2)
ggplot(km_all_center_melted, aes(x = variable, y = value)) + geom_line(aes(color = cluster, group = cluster))










