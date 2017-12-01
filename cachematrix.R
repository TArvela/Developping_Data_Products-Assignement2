## Put comments here that give an overall description of what your
## functions do

## This function is based on the example given for assignement 2 and adapted to find the inverse matrix

## makeCacheMatrix function 
makeCacheMatrix <- function(x = matrix()) {
  ##Define "m" variable
  cached <- NULL
  
  ## Set function => Store a Matrix
  set <- function(y) {
    x <<- y
    cached <<- NULL
  }
  
  ## Get function => returns the stored matrix
  get <- function() x
  
  ## Setsolve => Stores the solved matrix into the cache
  setsolve <- function(solve) cached <<- solve
  
  ## Getsolve => Returns the solved matrix
  getsolve <- function() cached
  
  ##List containing every function
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve will return the solution (inverse) matrix. If the matrix is the same as previously it will return the cached answer.

cacheSolve <- function(x, ...) {
  ## Defining new variable based on variable 
  cached <- x$getsolve()
  ## if "cached" variable is valid (is not NULL) it returns this cached answer, else it will calculate a new answer
  if(!is.null(cached)) {
    message("getting cached data")
    return(cached)
  }
  ##data is assigned to the problem matrix
  data <- x$get()
  ## cached contains the answer to the matrix
  cached <- solve(data, ...)
  ##New cached answer is defined for later usage
  x$setsolve(cached)
  return(cached)
}

##To use the function please use for instance "cacheSolve(makeCacheMatrix(a))"