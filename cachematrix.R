## We need to use the functions "makeCacheMatrix" and "cacheSolve" in order to
## solve the inverse of a matrix within the lexical scope of a function avoiding
## excess computation.

## This function allows me to set and get the values 
## of the matrix and the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              #Initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}   #Obtaining the matrix
  setInverse <<- function(inverse) {inv <<- inverse}   #Obtaining the inverse of the matrix
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function allows me to get the cache data.

cacheSolve <- function(x, ...)  #Gets cache data
  {
  inv <- x$getInverse()
  if(!is.null(inv)){     #Checking if the inverse matrix is NULL
    message("getting cached data")
    return(inv)     #Returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)   #Calculates inverse values
  x$setInverse(inv)
  inv
}

