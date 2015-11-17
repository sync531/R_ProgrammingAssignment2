## makeCacheMatrix creates a special matrix object that can cache its inverse
## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## above. If the inverse has already been calculated and the matrix has not changes,
## then cacheSolve should retrieve the inverse from the cache

# EXAMPLE:

# mat1 <- matrix(1:4, ncol = 2)
# cacheMatrix <- makeCacheMatrix(mat1)
# cacheSolve(cacheMatrix)  # This will return inverse of 'mat'

# mat2 <- matrix(4:7, ncol= 2)
# cacheMatrix$set(mat2) # This will change the matrix being cached
# cacheMatrix$get() # This will return the matrix being cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}

