
## Below functions can calculate the inverse of the matrix and cache the result.
## This cached result can be re-used if the inverse logic is to be computed repeatedly


## This functon can be used to cache the inverse of the matrix result
## Function formal argument input is a matrix
## Output is a list with result from 4 other defined functions
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    
    x <<- y
    i <<- NULL
    
  }
  
  get <- function() x
  setSolve <- function(inverse) i <<- inverse
  getSolve <- function() i
  
  list(set = set, get = get
       , setSolve = setSolve,
       getSolve = getSolve)
}


## This function will compute the inverse of the matrix
cacheSolve <- function(x, ...)
{
  ## Get matrix inverse from buffer
  m_i <- x$getSolve()
  
  ## if matrix inverse exists in the buffer
  if (!is.null(m_i)) {
    message("Getting Cache data")
    return(m_i)
  }
  
  ## Call get() to return the original matrix
  data <- x$get()
  
  ## Compute matrix inverse
  m_i <- solve(data)
  
  ## Set the matrix inverse value in inv object
  x$setSolve(m_i)
  
  
  m_i
  
}