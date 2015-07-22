## makeCacheMatrix creates a special object that stores a numeric vector
## it takes input x, which is a matrix
## set and get the value of the vector and inverse
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of a special vector created in the makeCacheMatrix function IF it has not already been calculated
## if the inverse has already been calculated, the inverse is taken from the cache and the computation is skipped
## returns a matrxi that is the inverse of R
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
