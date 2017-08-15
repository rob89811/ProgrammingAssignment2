makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
    set <- function(y) {
        x <<- y
      i <<- NULL
     }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() inverse
  list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)
  }

  
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
      i <- x$getinv()
   if(!is.null(i)) {
    message("getting the cached data")
        return(i)
        }
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
  }
  