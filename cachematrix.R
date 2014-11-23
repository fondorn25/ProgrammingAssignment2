## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##value of S gets set to NULL when new matrix is created
  s <- NULL
        
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
        ##get function returns value of original matrix
  get <- function() x
        ##sets S value when called by cacheSolve
  setsolve <- function(solve) s <<- solve
        ##gets cached S value when called by cacheSolve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
        ##check for value of S - if S is not NULL - cached data is pulled
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
        ##if S is NULL - S is being set to inverse of the matrix
  data <- x$get()
  s <- solve(data, ...)
        ##Calculated S value is being cached with setsolve
  x$setsolve(s)
  s
}
