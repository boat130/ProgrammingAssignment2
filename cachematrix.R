## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function
# Get the matrix and store it inside the function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
      InvMatrix <- NULL
      set <- function(y){
            x <<- y
            InvMatrix <<- NULL
      }
      get <- function() x
      setcache <- function(inverse) InvMatrix <<- inverse
      getcache <- function() InvMatrix
      list(set = set, get = get, setcache = setcache, getcache = getcache)
}


## Write a short comment describing this function
# If there is an inverse matrix stored in the makeCacheMatrix, this function will take the inverse in the function instead of calculating it again

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      InvMatrix <- x$getcache()
      if(!is.null(InvMatrix)){
            message("getting cache data")
            return(InvMatrix)
      }
      data <- x$get()
      InvMatrix <- solve(data, ...)
      x$setcache(InvMatrix)
      InvMatrix
}


# Test the function
test1 <- matrix(c(1/2,-1,-1/4,3/4),2,2, byrow = TRUE)
e <- makeCacheMatrix(test1)
cacheSolve(e)

test2 <- matrix(c(5/8,-1/8,-7/8,3/8),2,2)
e <- makeCacheMatrix(test2)
cacheSolve(e)
cacheSolve(e) ### You will see "getting cache data" because the function does get the data from the cache
