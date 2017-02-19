# We are going to use the iris set.  To make things a bit easier, I'm just going to tweak the data so that we are looking only at 
# virginica and versicolor.  I'm going to select 5 from virginica and 50 from versicolor:

set.seed(13)
iris$Species<-as.character(iris$Species)

imb_iris <- iris[ c(   which(iris$Species=="versicolor"),
            sample(which(iris$Species=="virginica"),size = 5,replace = F)),]

imb_iris$Species[which(imb_iris$Species=="virginica")]<-1
imb_iris$Species[which(imb_iris$Species=="versicolor")]<-0
imb_iris$Species<-as.numeric(imb_iris$Species)

# Rename the virginicolor class as 1 and versicolor as 0.

# Oversampling example - copy the original versicolor data enough times so that we have parity with virginica:

ovsmpl_iris <-rbind(imb_iris, sapply(X = imb_iris[which(imb_iris$Species==1),],FUN = rep,times =9))
table(ovsmpl_iris$Species)
  
  
# Undersampling example:  Cut down on the observations of versicolor (0).  Obviously this is ridiculous.
# You'd only do this on a huge dataset:

undsmp_iris <- imb_iris[ c(   which(imb_iris$Species==1),
                          sample(which(imb_iris$Species==0),size = 5,replace = F)),]



# Implementing SMOTE:  Synthetic Minority OVersampling Technique
# SMOTE uses several different steps:
# [1]  Find the K-nearest neighbor of each sample point.
# [2]  Subtract the feature vector of the nearest neighbor from the sample point.
# [3]  Multiple this vector by a random number between 0 and 1 add it to the new minority sample.

# For this simple example, I am going to set the defaultson this functions so that it generates a minority class which
# is at parity with the majority class.

# The 2-norm will be employed.

# Compute the n-norm:

k_norm <- function(x,y,k=2,i){
  return(  (sum((x-y[i,])^k))^(1/k)   )
}

# Function to find the nearest neighbor to a particular observation:
# Inputs:
# obs - A number indicating the row of the observation we want to find the nearest neighbor of.  
# data - A dataframe which the observations come from.
# k    - A number denoting the metric we wish to use.
kn_nearest <- function(obs,data,k=2){
  metrics <- sapply(X = c(1:dim(data)[1])[-obs],FUN = k_norm,x=data[obs,],y=data,k=k)
  min_index <- which(metrics==min(metrics))
  return(data[min_index,])
  }

# This simple example just assumes the user wants parity with the majority class:

SMOTE <- function(data,Class_Column,positive_class =1,negative_class = 0, parity = 1){
# [1]
imbalance <- sum(data[Class_Column]==0)/sum(data[Class_Column]==1)
min_set <- which(data[Class_Column]==1)
min_set2 <- sample( rep(min_set,(imbalance-1)) , size = sum(data[Class_Column]==0)*parity-sum(data[Class_Column]==1) ,replace = T  )

# [2]
gen_artf_obs <- function(i,data,k=2){
  min_neighbors <- kn_nearest(obs=i,data = data)    
  dist_neighbor <-  min_neighbors-data[i,]
  # [3] 
  artf_obs <- dist_neighbor*runif(min = 0 ,max=1,n=1) + data[i,]
}  

artf_samples <- do.call(what = rbind,lapply(X = min_set2,FUN = gen_artf_obs,k=2,data = data[, c(1:dim(data)[2])[-Class_Column]]   )   )
artf_samples[Class_Column]<-1
# Label the artificial and original observations.  This is more for pedagogical purposes.
artf_samples$Obs_Type <- "Artf."
data$Obs_Type <- "Orig."

names(artf_samples) <-names(data)
SMOTED_Frame <-  rbind(data,artf_samples)
return(SMOTED_Frame)
}

smote_iris <- SMOTE(data = imb_iris,Class_Column = 5,parity = .8)


jpeg(filename = "plot1.jpeg")
plot(smote_iris[(smote_iris$Species==1 & smote_iris$Obs_Type=="Orig."),1:2],col="blue",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[2]),max(smote_iris[2])))
par(new = TRUE)
plot(smote_iris[(smote_iris$Species==1 & smote_iris$Obs_Type=="Artf."),1:2],col="green",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[2]),max(smote_iris[2])))
par(new = TRUE)
plot(smote_iris[(smote_iris$Species==0 ),1:2],col="red",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[2]),max(smote_iris[2])))
dev.off()


jpeg(filename = "plot2.jpeg")
plot(smote_iris[(smote_iris$Species==1 & smote_iris$Obs_Type=="Orig."),c(1,3)],col="blue",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[3]),max(smote_iris[3])))
par(new = TRUE)
plot(smote_iris[(smote_iris$Species==1 & smote_iris$Obs_Type=="Artf."),c(1,3)],col="green",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[3]),max(smote_iris[3])))
par(new = TRUE)
plot(smote_iris[(smote_iris$Species==0 ),1:2],col="red",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[3]),max(smote_iris[3])))
dev.off()

jpeg(filename = "plot3.jpeg")
plot(smote_iris[(smote_iris$Species==1 & smote_iris$Obs_Type=="Orig."),c(1,4)],col="blue",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[4]),max(smote_iris[4])))
par(new = TRUE)
plot(smote_iris[(smote_iris$Species==1 & smote_iris$Obs_Type=="Artf."),c(1,4)],col="green",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[4]),max(smote_iris[4])))
par(new = TRUE)
plot(smote_iris[(smote_iris$Species==0 ),c(1,4)],col="red",xlim = c(min(smote_iris[1]),max(smote_iris[1])),ylim = c(min(smote_iris[4]),max(smote_iris[4])))
dev.off()





     