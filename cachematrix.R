## these two functions can be used to cache the inverse matrix
## 

## The fist fuction creat a matrix which cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse.cache <- matrix() ## creat an empty matrix for the inverse matrix, the value is NA 
  set <- function(y) { ## set function is used to change the matrix when required 
    x <<- y
    inverse.cache <<- matrix() ## once the matrix changed, the inverse matrix will set to empty
  }
  get <- function() x  ## show the values of the current matrix
  set.inverse <- function(inverse) inverse.cache<<- inverse ## cache the inverse matrix
  get.inverse <- function() inverse.cache ## show the inverse matrix
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## the second function compute the inverse matrix if it is empty and cache it, otherwise it will take the cached one

cacheSolve <- function(x, ...) {
  cache <- x$get.inverse() ## take the cashed matrix
  if(!all(is.na(cache))) { ## if there is already an inverse matrix, return it
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$set.inverse(cache)
  cache     ## otherwise, compute the inverse, cache and return it
}
