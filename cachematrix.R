## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getmean()
      if(!is.null(m)){
            message("getting cache data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setmean(m)
      m
}

test1 <- matrix(c(1/2,-1,-1/4,3/4),2,2, byrow = TRUE)
e <- makeCacheMatrix(test1)
cacheSolve(e)

test2 <- matrix(c(5/8,-1/8,-7/8,3/8),2,2)
e <- makeCacheMatrix(test2)
cacheSolve(e)
