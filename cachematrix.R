#function makeCacheMatrix----
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # store the Inverse (anfangs leer)
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset, wwhen new matrix is set
  }
  
  get <- function() x  # take back matrix
  
  setinverse <- function(inverse) inv <<- inverse  # save Inverse 
  
  getinverse <- function() inv  # call up Inverse 
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

#function cacheSolve----
cacheSolve <- function(x, ...) {
  # Attempt to retrieve the cached invers
  inv <- x$getinverse()
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  # Retrieve the original matrix
  data <- x$get()
  # Compute the inverse of the matrix
  inv <- solve(data, ...)  
  # Cache the inverse for future use
  x$setinverse(inv)
  
  inv
}

#Test----
# create a large matrix (100x100)
set.seed(42)
big_matrix <- matrix(rnorm(10000), nrow = 100, ncol = 100)

# Make sure that it is invertible (e.g. by cross product)
big_matrix <- t(big_matrix) %*% big_matrix

# Create the special matrix object with cache
cached_big_matrix <- makeCacheMatrix(big_matrix)

# Calculate the inverse
system.time(inv1 <- cacheSolve(cached_big_matrix))

# now fro the cache
system.time(inv2 <- cacheSolve(cached_big_matrix))

# Check if both are identical
identical(inv1, inv2)  # shold be true
