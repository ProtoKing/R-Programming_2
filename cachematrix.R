##Cashing the Invesrse of a Matrix


## This function creates a special "matrix" object that 
##  can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  Inv_matrix <- NULL      ## Inverse matrix initialization
  
  set <- function(y){     ## Defining set function of matrix
    x <<-y
    Inv_Matrix <<- NULL
  }
  
  get <- function(){ x     ## Defining return matrix object
                        
    Inv_set <- function(inverse) Inv_Matrix <<- inverse
    Inv_get <- function() Inv_Matrix
    list(set = set,get=get,Inv_set=Inv_set,Inv_get=Inv_get)
  }
   
}

## This function computes the inverse of the resulting matrix 
## created by the above function. If the inverse has already
##  been calculated, then the cachesolve should retrieve the 
## inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv_Matrix <- x$Inv_get()
  if (!is.null(Inv_Matrix)){
    message("getting cached data")
    return(Inv_Matrix)
  }
  mat <- x$get()
  Inv_Matrix <- solve(mat,...)
  x$setInverse(Inv_Matrix)
  Inv_Matrix
}
