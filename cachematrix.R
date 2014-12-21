
## the function "makeCacheMatrix" creates a special "matrix" wich return a list of functions

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL # in s is kept an inverse matrix of x
    set <- function (y) { # this function is used to set a new values of parametr x and drops s (inverse matrix that was calculated before) in value NULL.
      x <<- y
      s <<- NULL
    }
    get <- function() x #this function returns the x value
    setsolve <- function (slv) s<<-slv #this function sets s (the object from parent envirement) in value slv (in this case it will be used to set inverse matrix)
    getsolve <- function () s #this function returns s
    list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The following function calculate inverse matrix or get it from cache if it was calculated before

cacheSolve <- function(x, ...) {
    s <- x$getsolve () #to assign s an inverse matrix
    if (!is.null (s)) { #if inverse matrix was calculated then it returns s and doesn't make any calculation
      message ("getting cashed data")
      return (s)
    }
    data <- x$get () # to get value of a matrix x
    s <- solve(data, ...) #s is assigned an inverse matrix of data (x)
    x$setsolve (s)  # save an inverse matrix of x in a cache
    s
  
}
