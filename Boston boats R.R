#Step 0: Create a new R code script file, and Save this script file to your designated course folder. Name it as "Group assignment 7variables.R"
#--------------------------------------------------------#
#Step 1: Import the Data 
#--------------------------------------------------------#
setwd("C:/Users/user/Desktop/Dataset")
DT=read.csv("GroupProject_Boats_7variables.csv")
#Step 2: Check Regularities in the Data
#--------------------------------------------------------#
head(DT,5)
dim(DT)
summary(DT)
summary(DT$Q1.25) #to confirem, just in case
#--------------------------------------------------------#
#Step 3: Scale the Data if Necessary
#Well, I don't think this time it is necessary
#--------------------------------------------------------#
#Step 4: Select Segmentation Variables
#I did it once with 29Variables, and also with 3segments then I could elimante very similar variables. Condense them to 7
Segment = DT[,c("Q1.1","Q1.2","Q1.4","Q1.11","Q1.13","Q1.19","Q1.25")]
#View(Segment)
#Segment = DT[,2:8]
#View(Segment)
#--------------------------------------------------------#
#Step5: Define Similarity Measure#
#EuclideanD <- dist(Segment[1:2813, 1:7])
EuclideanD <- dist(Segment[1:10, 1:7])
EuclideanD = as.matrix(EuclideanD) 
View( round(EuclideanD, 1) )
#--------------------------------------------------------#
#Step6:Clustering methods Dendrogram#
EuclideanD = dist(Segment[, 1:7])
Hierarchical_Cluster = hclust(EuclideanD)
plot(Hierarchical_Cluster)
#Heatmap
heatmap(as.matrix(Segment[, 1:7]))
#Membership
HC_boat= as.vector(cutree(Hierarchical_Cluster, k = 3))
YD_HC1 = cbind(DT, HC_boat)
head(YD_HC1)
#--------------------------------------------------------#
# Step7: Profiling and interpretation of the segments
prop.table(table(YD_HC1$HC_boat))
Mean_by_Group <- function(data, groups) { aggregate(data, list(groups), function(x) mean(as.numeric(x))) } 
Group_Character_HC= Mean_by_Group(YD_HC1, YD_HC1$HC_boat)
View(Group_Character_HC)
write.csv(Group_Character_HC, "Group_Character_HC7-1.csv", row.names = FALSE)
#--------------------------------------------------------#

#R Code with 29 all Variables#
#Step 0: Create a new R code script file, and Save this script file to your designated course folder. Name it as "Group assignment.R"
#--------------------------------------------------------#
#Step 1: Import the Data 
#--------------------------------------------------------#
setwd("C:/Users/user/Desktop/Dataset")
DT=read.csv("GroupProject_Boats.csv")
#Step 2: Check Regularities in the Data
#--------------------------------------------------------#
head(DT,5)
dim(DT)
summary(DT)
summary(DT$Q1.29) #to confirem, just in case
#--------------------------------------------------------#
#Step 3: Scale the Data if Necessary
#Well, I don't think this time it is necessary
#--------------------------------------------------------#
#Step 4: Select Segmentation Variables
#with 29Variables, 
Segment = DT[,c("Q1.1","Q1.2","Q1.3","Q1.4","Q1.5","Q1.6","Q1.7","Q1.8","Q1.9","Q1.10","Q1.11","Q1.12","Q1.13","Q1.14","Q1.15","Q1.16","Q1.17","Q1.18","Q1.19","Q1.20","Q1.21","Q1.22","Q1.23","Q1.24","Q1.25","Q1.26","Q1.27","Q1.28","Q1.29")]
#--------------------------------------------------------#
#Step5: Define Similarity Measure#
#EuclideanD <- dist(Segment[1:10, 1:29]) 
EuclideanD <- dist(Segment[1:2813, 1:29]) 
EuclideanD = as.matrix(EuclideanD) 
View(EuclideanD) 
View( round(EuclideanD, 1) )
#--------------------------------------------------------#
#Step6:Clustering methods Dendrogram#
EuclideanD = dist(Segment[, 1:29])
Hierarchical_Cluster = hclust(EuclideanD)
plot(Hierarchical_Cluster)
#Heatmap
heatmap(as.matrix(Segment[, 1:29]))
#heatmap(as.matrix(Segment1[,c("Q1.2","Q1.4","Q1.5","Q1.6","Q1.7","Q1.9","Q1.10","Q1.13","Q1.14")]))
#Membership
HC_boat1= as.vector(cutree(Hierarchical_Cluster, k = 3))
YD_HC1 = cbind(Segment, HC_boat1)
head(YD_HC1)
#--------------------------------------------------------#
# Step7: Profiling and interpretation of the segments
prop.table(table(YD_HC1$HC_boat1))
Mean_by_Group <- function(data, groups) { aggregate(data, list(groups), function(x) mean(as.numeric(x))) } 
Group_Character_HC= Mean_by_Group(YD_HC1, YD_HC1$HC_boat1)
View(Group_Character_HC)
write.csv(Group_Character_HC, "Group_Character_HC29.csv", row.names = FALSE)

#R Code with Categorized Variables#
#Step 0: Create a new R code script file, and Save this script file to your designated course folder. Name it as "Group assignment.R"
#--------------------------------------------------------#
#Step 1: Import the Data 
#--------------------------------------------------------#
setwd("C:/Users/user/Desktop/Dataset")
DT=read.csv("GroupProject_Boats.csv")
#Step 2: Check Regularities in the Data
#--------------------------------------------------------#
head(DT,5)
dim(DT)
summary(DT)
summary(DT$Q1.29) #to confirem, just in case
#--------------------------------------------------------#
#Step 3: Scale the Data if Necessary
#Well, I don't think this time it is necessary
#--------------------------------------------------------#
#Step 4: Select Segmentation Variables
#I did it once with 29Variables, but it is too many, so this Time I categorized the Variables
#Group1- Inelastic, but not an expert: Q3, 4, 5, 6, 7, 9, 10, 13, 14
#Group2- Inelastic, expert: Q20, 23, 25, 26, 27, 28, 29
#Group3- Moderate, expert, outgoing: Q8, 11, 12, 15, 16, 17, 18, 19, 21, 22 
#Group4- Elastic or moderate: Q1, 2, 8, 11, 12, 15, 16, 17, 18, 19, 21, 22
#Through this segmentation Q1,2 are not appropriate to use
Segment1 = DT[,c("Q1.2","Q1.4","Q1.5","Q1.6","Q1.7","Q1.9","Q1.10","Q1.13","Q1.14")]
Segment2 = DT[,c("Q1.20","Q1.23","Q1.25","Q1.26","Q1.27","Q1.28","Q1.29")]
Segment3 = DT[,c("Q1.8","Q1.11","Q1.12","Q1.15","Q1.16","Q1.17","Q1.18","Q1.19","Q1.21","Q1.22")]
#--------------------------------------------------------#
#Step5: Define Similarity Measure#
EuclideanD <- dist(Segment1[1:10, 1:9]) 
#EuclideanD <- dist(Segment1[1:2813, 1:9]) 
EuclideanD = as.matrix(EuclideanD) 
View(EuclideanD) 
View( round(EuclideanD, 1) )
#--------------------------------------------------------#
#Step6:Clustering methods Dendrogram#
EuclideanD = dist(Segment1[, 1:9])
Hierarchical_Cluster = hclust(EuclideanD)
plot(Hierarchical_Cluster)
#Heatmap
heatmap(as.matrix(Segment1[, 1:9]))
#heatmap(as.matrix(Segment1[,c("Q1.2","Q1.4","Q1.5","Q1.6","Q1.7","Q1.9","Q1.10","Q1.13","Q1.14")]))
#Membership
HC_boat1= as.vector(cutree(Hierarchical_Cluster, k = 3))
YD_HC1 = cbind(Segment1, HC_boat1)
head(YD_HC1)
HC_boat2= as.vector(cutree(Hierarchical_Cluster, k = 3))
YD_HC2 = cbind(Segment2, HC_boat2)
head(YD_HC2)
HC_boat3= as.vector(cutree(Hierarchical_Cluster, k = 3))
YD_HC3 = cbind(Segment3, HC_boat3)
head(YD_HC3)
#--------------------------------------------------------#
# Step7: Profiling and interpretation of the segments
prop.table(table(YD_HC1$HC_boat1))
Mean_by_Group <- function(data, groups) { aggregate(data, list(groups), function(x) mean(as.numeric(x))) } 
Group_Character_HC= Mean_by_Group(YD_HC1, YD_HC1$HC_boat1)
View(Group_Character_HC)
write.csv(Group_Character_HC, "Group_Character_HC.csv", row.names = FALSE)
#--------------------------------------------------------#
#Step6: K-means Clustering Method
set.seed(777) 
kmeans_Cluster <- kmeans(Segment1[, 1:9],centers = 3, iter.max=2000) 
Kmeans_boat1 = as.vector(kmeans_Cluster$cluster) 
YD_Kmeans1 = cbind(Segment1, Kmeans_boat1)
head(YD_Kmeans1)
