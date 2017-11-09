## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix -- This function creates a special "matrix" object that can cache its inverse
#
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of matrix
# 4. get the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setInverse <- function(invMatrix) minv <<- invMatrix
    getInverse <- function() minv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#caseSolve function inverse the passed matrix. It first try to get it from cache, 
#if not found calculates inverse and assigns to cache.

cacheSolve  <- function(x) {
    minv <- x$getInverse()
    if(!is.null(minv)) {
        message("getting cached data...")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data)
    x$setInverse(minv)
    minv
}



##example:
## > matr<-matrix(c(4,7,2,6),nrow=2,ncol=2)
## > m=makeCacheMatrix(matr)
## > m$get()
## > mm$get()
##      [,1] [,2]
## [1,]    4    2
## [2,]    7    6

## executing first time so matrix is not cached
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4


## executing second time so returned matrix is cached
## getting cached data...
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4
