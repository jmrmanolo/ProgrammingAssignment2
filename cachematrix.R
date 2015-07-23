## Caches the inverse of a Matriz
## Assignment for Programming with R

## Creates the matrix

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) cache <<- matrix
  getmatrix <- function() cache
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



## Caches the inverse

cacheSolve <- function(x, ...) {
  cache <- x$getInverse()
  
  
  if (!is.null(cache)) {
    message("getting cached data")
    
    return(cache)
  }
  
  matrix <- x$get()
  
  
  tryCatch( {
    cache <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    x$setMatrix(cache)
  } )
  
  return (cache)  
}
