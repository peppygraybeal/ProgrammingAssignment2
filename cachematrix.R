## These two functions compute and store the inverse of a square matrix.
## function makeCacheMatrix creates a list of all four functions needed to do this.
## set() saves the original matrix
## get() retrieves the original matrix
## getinverse() retrieves the inverse
## setinverse() caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixDimensions <- dim(x)
  set <- function(y) {
    m <<- matrix(, nrow=matrixDimensions[1], ncol=matrixDimensions[2])
    x <<- y
  }
  set(x)
  get <- function() x
  getinverse <- function() m           
  setinverse <- function(inverse) m <<- inverse
  assign("listVector", list(set = set, get = get,
                            setinverse = setinverse,
                            getinverse = getinverse), envir = .GlobalEnv)
}


## function cacheSolve actually calls the four functions
## defined in the previous function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <<- listVector$getinverse()
  if (all(!is.na(m))) {
    message("getting cached data") 
    return(m)
  }
  data <- listVector$get()          
  m <<- solve(data)
  listVector$setinverse(m)
}
