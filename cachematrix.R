## Program: cachematrix.R
### The functions below implement inverse matrix calculation, utilizing 
### the deep assignment operator (<<-) to achieve object caching. The
### caching ensures that the inverse calculation is performed only once,
### and the function will return the cached inverse upon future calls.

#' @title makeCacheMatrix: Creates a special matrix object for use with \code{cacheSolve}
#' @description This function constructs a special "object" encapsulating
#' a matrix, with functions for getting/setting the matrix and cached inverse.
#' It is meant to be used in conjunction with \code{cacheSolve}. 
#' @details The functions provided with this special matrix object are:
#' * get (gets the matrix associated with this object)
#' * set (assigns a matrix to the object)
#' * getinv (gets the cached inverse matrix)
#' * setinv (assigns an inverse matrix to the object for caching)
#' @param x A \code{matrix} object
#' @return A \code{list} representing the special matrix object
#' @keywords manip
#' @examples
#' x <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))
makeCacheMatrix <- function(x = matrix()) {
  
  cachedInv <- NULL
  set <- function(y) {
    x <<- y
    cachedInv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cachedInv <<- inv
  getinv <- function() cachedInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#' @title cacheSolve: Calculates the inverse of a matrix object returned by \code{makeCacheMatrix}
#' @description This function calculates the inverse of a matrix returned by \code{makeCacheMatrix}.
#' If the inverse calculation has already been performed, then the cached inverse matrix is used.
#' Otherwise, the inverse matrix is calculated using the \code{solve} function.
#' @details A message is displayed when the cached version of the inverse is used.
#' @param x A special matrix object returned by \code{makeCacheMatrix}
#' @param ... Additional arguments to pass to the \code{solve} function
#' @return A \code{matrix} representing the inverse
#' @keywords manip
#' @examples
#' x <- makeCacheMatrix(matrix(c(4,3,3,2), nrow=2, ncol=2))
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

