## Put comments here that give an overall description of what your
## functions do
##
## Overall Description 
##    Matrix inversion can be a costly computation so the use
##    of caching to obtain the inverse of a matrix.  
##    The following two funcrions are used to cache the 
##    inverse of a matrix.
##
## Write a short comment describing this function
##
##  The makeCacheMatrix creates a list containing a function to
##   1. Set the value of the matrix
##   2. Get the value of the matrix
##   3. Set the value of the inverse of the matrix
##   4. Get the value of the inverse of the matrix  
##
makeCacheMatrix <- function(x = matrix()) {
  cacheinv <- NULL
  set <- function (y) {
    x <<- y
    cacheinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cacheinv <<- inverse
  getinverse <- function() cacheinv
  list(set=set,get+get,setinverse=setinvers,getinverse=getinverse)
}


## Write a short comment describing this function
##
##  The following function will return the inverse of the matrix.
##  First we check to see if the inverse has already been computed.
##  If the inverse has been computed, we get the result.
##  If the inverse has not been computed, we use the setinverse 
##  function to obtain the inverse of the cached data. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheinv < x$getinverse()
  if (!is.null(inv)) {
    message("obtaining cached data,")
    return(cacheinv)
  }
  data <- x$get()
  cacheinv <- solve(data)
  x$setinverse(cacheinv)
  cacheinv
}
##
## Create a function where x is numeric, m is set to NULL so 
## when the vector has changed the mean is recalculated
##
makeVector <- function(x = numeric()) {
  m <- NULL  
  set < function(y) {
    x <<- y
    m <<- NULL
  }
  get <-function () x
  setmean <- function (mean) m <<- mean
  getmean <- function () m
  list (set=set,get=get,setmean=setmean,getmean=getmean)
  }
##
## Create a function which checks to see if the mean has already been 
## calculated and it will get the mean if it has been calculated
## or it will perform the calculation if necessary
##
cachemean <- function(x, ...) {
  m <- x$getmean()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
