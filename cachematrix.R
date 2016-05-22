## Pair of functions used to cache the inverse of matrix (a usually costly computation)
##
## Usage:
##
##  > invertibleMatrix <- makeCacheMatrix( rbind(c(10, 20), c(30, 40)) )
##  > invertibleMatrix$get()
##         [,1] [,2]
##    [1,]   10   20
##    [2,]   30   40
##  > cacheSolve(invertibleMatrix)
##          [,1]  [,2]
##    [1,] -0.20  0.10
##    [2,]  0.15 -0.05
##  > cacheSolve(invertibleMatrix)
##    returning cache data
##          [,1]  [,2]
##    [1,] -0.20  0.10
##    [2,]  0.15 -0.05
##
## Note that the second 'cacheSolve' call uses data cached from the first 'cacheSolve'

## makeCacheMatrix: a structure that stores an invertible matrix 'm' and its inverse (once set)

makeCacheMatrix <- function(m = matrix()) {
  cache <- NULL
  set <- function(n) {
    m <<- n
    cache <<- NULL
  }
  get <- function() m
  setcachedinverse <- function(inverse) cache <<- inverse
  getcachedinverse <- function() cache

  list(set = set, get = get, setcachedinverse = setcachedinverse, getcachedinverse = getcachedinverse)
}


## cacheSolve: returns a matrix that is the inverse of the matrix stored
## within a 'makeCacheMatrix' structure.

cacheSolve <- function(m, ...) {
  cache <- m$getcachedinverse()
  if(!is.null(cache)) {
    message("returning cache data")
    return(cache)
  }
  data <- m$get()
  cache <- solve(data, ...)
  m$setcachedinverse(cache)
  cache
}
