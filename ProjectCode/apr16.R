#apr16

#1. initialize group centroids
  #choose 3 data points randomly; can also partition data into k parts and take centroid of each partition
#2. repeat 2 steps until nothing changes
  #a. assign each data point to the nearest group centroid
    #for i=1...n let group indicator variable c_i=argmin_j|x_i - u_j| (argmin means which j minimizes the value)
  #b. update the group centroids
    #for j=1...k:   u_j=(1/(num of points))sum_i|c_i=j(x_i)

#remember to standardize/normalize columns

kmeansBurf <- function(x, k){
  #scale
  x <- scale(x)
  # initialize
  centroids <- list()
  centroids <- x[sample(1:nrow(x), k), ]
  assignments <- numeric(nrow(x))
  old_assignments <- 0
  iterations <- 0
  # while nothing is changing
  while(!identical(assignments, old_assignments)){
    old_assignments <- assignments
    # make assignments based on the centroids
    assignments <- sapply(1:nrow(x), function(i){
      row <- x[i, ]
      return(best_class(row, centroids))
    })
    # update the centroids to be the new means
    for(class in 1:k){
      group <- x[which(assignments==class), ]
      centroids[class, ] <- new_centroid(group)
    }
    iterations <- iterations + 1
  }
  message(paste0(k, "-Means converged after ", iterations, " iterations."))
  return(assignments)
}

# returns the index of the row of the best centroid
best_class <- function(row, centroids){
  dists <- sapply(1:nrow(centroids), function(i){
    return(dist(rbind(row, centroids[i, ])))
  })
  return(which.min(dists))
}

new_centroid <- function(group_matrix){
  ret <- numeric(ncol(group_matrix))
  ret <- sapply(seq_along(ret), function(i){ mean(group_matrix[ ,i])})
  return(ret)
}

kmeans <- function(X, k) {
  n <- dim(X)[1]; p <- dim(X)[2]; centroids <- list()
  X <- scale(X)
  inds <- sample(1:n, k)
  for (i in 1:k) centroids[[i]] <- X[i,]
  
  oldGroups <- 1:n
  newGroups <- numeric(n)
  
  iter <- 0
  while(!identical(oldGroups, newGroups)) {
    iter <- iter + 1
    #assign new groups
    oldGroups <- newGroups
    for (i in 1:n) {
      distance <- sapply(centroids, function(c) sqrt(sum((X[i,] - c) ^ 2)))
      check <- which(distance == min(distance))
      if (length(check) == 0) {
        print(newGroups)
        print(iter)
      }
      newGroups[i] <- which(distance == min(distance))
    }
    
    #update centroids
    for (i in 1:k) centroids[[i]] <- colMeans(X[which(newGroups == i),])
  }
  
  return(newGroups)
}


anotherDF <- countryData[,c('latitude','longitude','mortality')]
X <- as.matrix(anotherDF)

k=5
for (i in 1:6) {
  groups <- kmeans(X, k)
  colores <- brewer.pal(k, "Set1")
  colores <- colores[groups]
  
  map("world", fill = TRUE, col = "grey")
  symbols(countryData$longitude, countryData$latitude,
          add = TRUE, inches = FALSE, circles = rep.int(2, length(countryData$latitude)),
          fg = colores, bg = colores)
}


