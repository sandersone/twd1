# silinfo  - list with silhouette width information, see partition.object.



# partition.object {cluster}   R Documentation
# Partitioning Object
# 
# Description
# 
# The objects of class "partition" represent a partitioning of a dataset 
# into clusters



# silinfo   
# a list with all silhouette information, only available when the number of 
# clusters is non-trivial, i.e., 1 < k < n and then has the following 
# components, see silhouette
# 
# widths
# an (n x 3) matrix, as returned by silhouette(), with for each observation 
# i the cluster to which i belongs, as well as the neighbor cluster of i 
# (the cluster, not containing i, for which the average dissimilarity 
# between its observations and i is minimal), and the silhouette width 
# s(i) of the observation.
# 
# clus.avg.widths
# the average silhouette width per cluster.
# 
# avg.width
# the average silhouette width for the dataset, i.e., simply the average of 
# s(i) over all observations i.
# 
# This information is also needed to construct a silhouette plot of the 
# clustering, see plot.partition.
# 
# Note that avg.width can be maximized over different clusterings 
# (e.g. with varying number of clusters) to choose an optimal clustering.




# silhouette {cluster}   R Documentation
# Compute or Extract Silhouette Information from Clustering
# 
# Description
# 
# Compute silhouette information according to a given clustering in k clusters.

# Details
# 
# For each observation i, the silhouette width s(i) is defined as follows: 
#    Put a(i) = average dissimilarity between i and all other points of the 
# cluster to which i belongs (if i is the only observation in its cluster, 
# s(i) := 0 without further calculations). For all other clusters C, 
# put d(i,C) = average dissimilarity of i to all observations of C. 
# The smallest of these d(i,C) is b(i) := \min_C d(i,C), and can be seen as 
# the dissimilarity between i and its “neighbor” cluster, i.e., the nearest 
# one to which it does not belong. Finally,
# 
# s(i) := ( b(i) - a(i) ) / max( a(i), b(i) ).



# INTERPRETACJA

# Observations with a large s(i) (almost 1) are very well clustered, a small
# s(i) (around 0) means that the observation lies between two clusters, and 
# observations with a negative s(i) are probably placed in the wrong cluster.





library(cluster)
## generate 25 objects, divided into 2 clusters.
x <- rbind(cbind(rnorm(50,0,0.5), rnorm(50,0,0.5)),
           cbind(rnorm(40,5,0.5), rnorm(40,5,0.5)))
pamx <- pam(x, 2)
str(pamx)
plot(pamx, which=2)

pamx$silinfo

cluster:::plot.partition

pamix <- pamx


plotDFobject <- function( frame, k ){
   stopifnot( is.data.frame( frame ), is.integer( k ) )
   plotPAMobject( pam( frame, k ) )
}

library(reshape2)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

plotPAMobject <- function( pamix ){
   stopifnot( any( class(pamix) == "partition" ) )
   frame <- as.data.frame( pamix$silinfo$widths )
   frame[4] <- row.names(frame)
   names(frame)[4] <- "observation"
   
   ladne_kolory <- sample(c("#00008B", "#8B3E2F", "#8B6508", "#66CD00", "#8B0000"))
   
   for (i in 1:length(unique(frame$cluster)) ){
      assign( paste0("frame",i), frame[frame$cluster==i,])
    
      assign( paste0("wykres",i), ggplot( get(paste0("frame",i)), aes( y = sil_width, x = observation ) )+
         geom_bar(stat="identity", colour="black", fill= ladne_kolory[2]) +
         coord_flip() +
         scale_x_discrete(limits= get(paste0("frame",i))$observation[order(get(paste0("frame",i))$sil_width)])+
         theme( axis.text.x = element_text(family = "mono", size = 15),
                axis.text.y = element_text(family = "mono", size = 15),
                axis.title.x= element_text(family = "mono", size = 20),
                axis.title.y= element_text(family = "mono", size = 20),
                title =element_text(family = "mono", size = 25)
         )+
         labs(x = "obserwacje", y="miara silhouette")+
         scale_colour_brewer(palette="Set1")   )
      
   }
   
   grid.newpage() 
   
   for (i in 1:length(unique(frame$cluster)) ){
   print(get(paste0("wykres",i)), vp=viewport(x=0.75, y = i/length(unique(frame$cluster)), 
                           width=0.5, height=1/length(unique(frame$cluster))))
   }
}

