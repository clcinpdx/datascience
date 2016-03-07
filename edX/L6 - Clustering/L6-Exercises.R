# Clustering algorithms, which are tailored
# to find similar customers or similar items,
# form the backbone of many of these recommendation systems.

setwd ('C:/Users/crcalder/Google Drive/eBay/Data-Science/datascience/edX/L6 - Clustering')

movies  <- read.table ("u.txt", sep = '|', header = FALSE, quote="\"")
str(movies)


# Add column names
colnames(movies) = c("ID","Title","ReleaseDate","VideoReleaseDate", "IMDB", "Unknown","Action", "Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")

# remove unneeded variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)  # To remove duplicates

str(movies)

table(movies$Comedy) #How many movies are classified as comedies?
table(movies$Western) # How many movies are classified as westerns?
table(movies$Romance,movies$Drama) # number of movies that are both Romance and Drama

# Hierchical clustering
# Step 1: determine the distances each of the genres are from each other. use dist function
# for columns 2:20

distances = dist(movies[2:20],method="euclidean")

# Now let's cluster our movies using the hclust function
# for hierarchical clustering. Use hclust where the first argument is distances,
# the output of the dist function.

clusterMovies = hclust(distances, method = "ward.D")

# The ward method cares about the distance between clusters using
# centroid distance, and also the variance in each of the clusters.
# Now let's plot the dendrogram of our clustering algorithm
# by typing plot, and then in parentheses clusterMovies.

plot (clusterMovies)

# label each of the data points according to what cluster it belongs
# to using the cutree function with 10 clusters. 

clusterGroups = cutree(clusterMovies,k=10)

# We'll use the tapply function to compute
# the percentage of movies in each genre and cluster.

tapply(movies$Action,clusterGroups,mean)
# several clusters have no Action

tapply(movies$Action,clusterGroups,mean)
# Some have 1.0 

# compare in a table the 20 in a category vs. cluster

# now lets determine what cluster Men in black is in.
subset (movies,Title=="Men in Black (1997)")

# So it looks like Men in Black is the 257th row in our data.
# So which cluster did the 257th movie go into?
# We can figure this out by typing clusterGroups[257].

clusterGroups[257]

# So "Men in Black" was in cluster 2. Let's look more at other movies in cluster 2

cluster2 = subset (movies,clusterGroups == 2)
cluster2$Title[1:10]

# from which we see that movies like GoldenEye an Stargate are good recommendations
# based on our clustering in the "Action / Adventure / SciFi" cluster

# Excerise: Run the cutree function again to create the cluster groups, but 
# this time pick k = 2 clusters. It turns out that the algorithm groups all 
# of the movies that only belong to one specific genre in one cluster (cluster 2)
#, and puts all of the other movies in the other cluster (cluster 1). 
# What is the genre that all of the movies in cluster 2 belong to?
clusterGroupsTwo = cutree(clusterMovies,k=2)
tapply(movies$Action,clusterGroupsTwo,mean)
# cluster 1
tapply(movies$Adventure,clusterGroupsTwo,mean)
# cluster 1
tapply(movies$Animation,clusterGroupsTwo,mean)
# cluster 1
tapply(movies$Childrens,clusterGroupsTwo,mean)
# cluster 1
tapply(movies$Comedy,clusterGroupsTwo,mean)
# cluster 1
tapply(movies$Crime,clusterGroupsTwo,mean)
# cluster 1
tapply(movies$Documentary,clusterGroupsTwo,mean)
# cluster 1
tapply(movies$Drama,clusterGroupsTwo,mean)
# cluster 1 & 2, where 2 is 1.0


# Unit 6: Seeing the Big Picture: Segmenting 
# Images to Create Data (Recitation) > Video 2: Clustering Pixels

flower  <- read.csv ("flower.csv", header = FALSE)
str(flower)

# We realize that the way the data is stored does not reflect that this 
# is a matrix of intensity values. Actually, R treats the rows as observations 
# and the columns as variables. Let's try to change the data type to a matrix
# by using the as.matrix function.

flowerMatrix = as.matrix(flower)
str(flowerMatrix)

# And now if we look at the structure of the flower matrix, we realize that 
# we have 50 rows and 50 columns. What this suggests is that the resolution 
# of the image is 50 pixels in width and 50 pixels in height.

# Hierachial Clustering 
# Step one: convert the matrix to a vector

flowerVector = as.vector(flowerMatrix)
str(flowerVector)

# now we have 2500 values, which is expected 50 x 50

# Step two: calculate distance matrix: which computes the pairwise distances 
# between all the intensity values in the flower vector.

distance = dist(flowerVector, method="euclidean")

# Step three: Now we can cluster the intensity values using hierarchical clustering.
# As a reminder, the Ward's method is a minimum variance method, which
# tries to find compact and spherical clusters. We can think about it as 
# trying to minimize the variance within each cluster and the distance among 
# clusters. 

clusterIntensity = hclust(distance,method="ward.D")

# STep four: plot Dendrogram

plot(clusterIntensity)

# looks like two or three clusters would be appropriate
# let's add rectangles to visually appreciate what three clusters 
# would look like

rect.hclust(clusterIntensity,k=3,border="red")

# creat the three clusters
flowerClusters = cutree(clusterIntensity,k=3)
flowerClusters
# And we see that flowerClusters is actually a vector that assigns each 
# intensity value in the flower vector to a cluster.
# It actually has the same length, which is 2,500, and has values 1, 2, 
# and 3, which correspond to each cluster.

tapply(flowerVector,flowerClusters,mean)

#          1          2          3 
# 0.08574315 0.50826255 0.93147713 

# where we see that cluster 1 mean is closest to zero, so it is the 
# darkest, and the third cluster is lightest.

# And now the fun part.
# Let us see how the image was segmented. To output an image, we can use the 
# image function in R, which takes a matrix as an input.
# But the variable flowerClusters, as we just saw, is a vector.
# So we need to convert it into a matrix. We can do this by setting the 
# dimension of this variable by using the dimension function.

dim(flowerClusters) = c(50,50)

image(flowerClusters, axes = FALSE)

# The darkest shade corresponds to the background,
# and this is actually associated with the first cluster.
# The one in the middle is the core of the flower, and this is cluster 2.
# And then the petals correspond to cluster 3, which has the fairest shade 
# in our image.

# now compare to original image

image(flowerMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))

##################################################################
# Video 4: MRI Image

healthy = read.csv("healthy.csv",header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# let's start by taking a look at the original image, before any 
# clustering

image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length=256)))

# And it shows different substances, such as the gray matter, the white matter,
# and the cerebrospinal fluid. Now let us see if we can isolate these substances
# via hierarchical clustering.

healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

# we get errors because it is going to require to much processing

# Therefore we will try to segment
# the MRI image using the k-means clustering algorithm.
# set k to reflect the number of clusters we hope for, which in this case
# reflects the now types of materials we expect in the image

k = 5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

# need to extract the cluster vector from KMC

healthyClusters = KMC$cluster

# Now how can we obtain the mean intensity value within each of our 5 clusters?

# this information is available in the str(KMC) called centers
# or
KMC$centers[1-5]

# Let us output the segmented image and see what we get.
# Recall that we first need to convert the vector healthy clusters to a matrix.
# To do this, we will use the dimension function, that takes as an input the healthyClusters vector.
# And now we're going to turn it into a matrix.

dim(healthyClusters) = c(nrow(healthyMatrix),ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col = rainbow(k))

############################################################3
## Detecting tumors

tumor = read.csv("tumor.csv",header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)
str(tumorMatrix)
install.packages("flexclust")
library(flexclust)

KMC.kcca = as.kcca(KMC,healthyVector)

tumorClusters = predict(KMC.kcca, newdata = tumorVector)

dim(tumorClusters) = c(nrow(tumorMatrix),ncol(tumorMatrix)))

image(tumorClusters, axes=FALSE, col=rainbow(k))