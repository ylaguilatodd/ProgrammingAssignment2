## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  z <- NULL
  
  dataset <- function(y){
    x <<- y
    z <<- NULL
  }
  
  getFun <- function()x
  
  setInverse <- function(inverse) z <<- inverse
  
  getInverse <- function() z 
  
  list(dataset = dataset, 
       getFun = getFun, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
  

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
 
  z <- x$getInverse()
  
  if(!is.null(z)){
    
    message("processing data")
  
    return(z)
  }
  
  mat <- x$get()
  
  z <- solve(mat,...)
  
  x$setInverse(z)
  
  z
  
}
