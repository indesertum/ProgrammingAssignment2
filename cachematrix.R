## This is a pair of funtions that caches the inverse of a matrix

## This function creates a matrix that caches its inverse. 
## get retrieves a matrix stored in the parent 
## set changes the matrix stored in the parent
## setinverse stores the value of the input in a variable called inv in the parent
## getinverse retrieves that value inv

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve calculates the inverse of the matrix created in makeCacheMatrix
## but the first thing it does is check whether that matrix exists in memory
## if it does exist it returns that value
## if it doesn't exist it calculates the inverse of the matrix
## it then stores that inverse in the object generated with the makeCacheMatrix and returns the value

cacheSolve <- function (x,...){
  inv <- x$getmean()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}