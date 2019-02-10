## MaskecacheMatrix Creates list to get/set matrix and get/set cached/inverted matrix, 
##cacheSolve Calculate inverted matrix, however checks in the first place if 
## matrix has been cached, if yes returns cached one. If not 
## calculate inverted matrix and caches it.

## Creates list to get/set matrix and get/set cached/inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) i <<- inv
  get_inv <- function() i
  list(set = set,
       get = get,
       set_inv = set_inv,
       get_inv = get_inv)


}


## Calculate inverted matrix, however checks in the first place if 
## matrix has been cached, if yes returns cached one. If not 
## calculate inverted matrix and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$get_inv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$set_inv(i)
  i
  
  
  
}
