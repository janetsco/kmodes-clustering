#Load necessary packages
library("clustMixType")
#library("VIM")
#library("visdat")
library(skimr)
library(FeatureImpCluster)
library("attempt")

#Read in the csv
data <- read.csv('/Users/janetscott/Documents/Articles/AutoInsurance.csv')

#Return the first few rows of data
head(data)

#Return the number of columns
ncol(data)

#Check for the percentage of missing values in each column
apply(data, 2, function(col)sum(is.na(col))/length(col))*100

#There don't appear to be any missing values.

#Return datatypes for each column
str(data)

#Below we create a copy of our data frame so we do not change the original table.
data2 <- data

#Scale numerical variables for improved clustering
data2[,c(3, 10, 13:17, 22)] <- lapply(data2[,c(3, 10, 13:17, 22)], function(x) c(scale(x)))

#Convert character fields to factors for some names in a vector named 'col_names'
col_nums <- c(1:2, 4:9, 11:12, 18:21,23:24)
data2[,col_nums] <- lapply(data2[,col_nums], factor)

#Verify that datatypes changed
str(data2)

#Check min nad max values for Total.Claim.Amount field
min(data2$Total.Claim.Amount)
max(data2$Total.Claim.Amount)

#Return descriptive statistics for numeric variables
summary(data2)

#Run algorithm to determine ideal number of clusters
Es <-numeric(10)
for(i in 1:10){
  kpres <- kproto(data2, k = i, nstart = 5)
  Es[i] <- kpres$tot.withinss
}
par(mfrow=c(1,1), mar=c(3,2,2,2))
plot(1:10, Es, type = "b", ylab = "Objective Function", xlab = "# Clusters",
     main = "Scree Plot")

summary(kpres)

#Compute k-prototypes clustering below.
kpres <- kproto(x = data2, k = 6, na.rm = TRUE)
kpres # output 1
summary(kpres) # output 2

library(wesanderson)
par(mfrow=c(1,1))
clprofiles(kpres, data, col = wes_palette("Royal1", 4, type = "continuous"))  # figure 1
clprofiles(kpres, data) # figure1

#Show plots in external window
dev.new(width=5, height=4, noRStudioGD = TRUE)
clprofiles(kpres, data2)

#Let's write the cluster numbers to our original data frame.
#cluster_list <- kpres$cluster

clust_list <- as.data.frame(kpres$cluster)
colnames(clust_list) <- c("Cluster.number")
data_w_clust <- cbind(clust_list, data)

head(data_w_clust)

#Group by cluster number using skimr package

group_by(data_w_clust, Cluster.number) %>%
  skim()

#Check for feature importance using FeatureImpCluster package and plot the results
data <- as.data.table(data2)
res <- kproto(x=data2, k=6)

FeatureImp_res <- FeatureImpCluster(res, data2)
plot(FeatureImp_res,data2,color="type")


