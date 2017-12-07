## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Homework assignment R Programming, Week 3, Programming assignment 2
## Course 2 of 10 in Data Science specialization
## "Lexical Scoping"    By Karen Enfield 12/6/2017

## First Create a matrix (ie: matrix(c(5/8,-1/8,-7/8,3/8),2,2), 
## and pass it through to makeCacheMatrix, which will return a new kind 
## of matrix with built in inversion and cache functionality.
##  Next, This new matrix can be passed to cacheSolve 
## (ie: cacheSolve(newmatrix) and see the inverse returned.
##  Finally, try calling cacheSolve(newmatrix) again and it should 
## indicate that it is getting the cached data, rather than recomputing
## it.
## Optionally, you can call the set function and set a new matrix,
## and calling cacheSolve should not get the data this time, but 
## compute it anew.  Whereupon, calling cacheSolve again will result
## in mereley getting the cached result, rather than computing it.

## makeCacheMatrix creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has lsnot changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    # no need to recompute
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Computing the inverse of a square matrix can be done with the 
  ## solve function in R. For example, if X is a square invertible matrix,
  ## then solve(X) returns its inverse.
  ## NOTE: SINGULARITY Matrixes wont work.  
  ## Please be sure a square matrix with values that don't result in 
  ## a singularity matrix are used (ie matrix(1:9,3,3) will NOT work
  ## for solve, as this is a singuarity or singular matrix)
  
  m <- solve(data)
  x$setinverse(m)
  m
}
