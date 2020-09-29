## These two functions generate a matrix which is capable of caching its
## inverse result, so that when we have already calculated it, we don't need
## to solve it again if we need the same result again (e.g. if we had to 
## invert it inside a loop)

## This function has getters and setters for the original matrix and its
## inverse. Using the <<- operator we can cache the value instead of using
## it as a normal variable

makeCacheMatrix <- function(x = matrix()) {
  
  inversematrix <- NULL
  
  getmatrix <- function() x
  setmatrix <- function(m) {
    x <<- m
    inversematrix <<- NULL
  }
  
  getinverse <- function() inversematrix
  setinverse <- function(i) {
    inversematrix <<- i
  }
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This functions calculates the inverse matrix, but before actually solving
## it, it checks if there is already a cached value it can use

cacheSolve <- function(x, ...) {
        
  inversematrix <- x$getinverse()
  
  if(!is.null(inversematrix)){
    print("cached matrix:")
    return(inversematrix)
  } else {
    matrix <- x$getmatrix()
    inversematrix <- solve(matrix)
    x$setinverse(inversematrix)
    print("no cached values found, caching the following result:")
    inversematrix
  }
  
}
