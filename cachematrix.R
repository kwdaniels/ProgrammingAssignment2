
## These functions allow the user to execute a processor-intensive matrix inversion
## computation once, then cache the results for future use so the full computation does
## not need to be performed again when called upon. 
## Sample usage:
##    a <- makeCacheMatrix(matrix(c(7,3,10,15),2,2))  #caches a matrix and returns a list of functions in "a"
##    a$get() #view the cached matrix above
##    cacheSolve(a)   #compute and return the inverse of the matrix cached above
##    cacheSolve(a)   #return the cached inverse of the matrix without having to recompute it
##    a$set(c(3,5,2,1),2,2) #change the cached matrix to a new matrix
##    cacheSolve(a)   #compute and return the inverse of the matrix cached above
##    cacheSolve(a)   #return the cached inverse of the matrix without having to recompute it

## Purpose of makeCacheMatrix: To store a square matrix and its inverse, and to make the 
##    stored matrix values available for future retrieval
## Parameter: a square matrix. This sets the initial matrix value when first called.
## Retured value: a list of functions:
##    set: establish a new matrix if needed after the initial matrix was set
##    get: return the current matrix values that have been set
##    setinverse: set/store the inverse of the current matrix
##    getinverse: retrieve the stored inverse of the current matrix

makeCacheMatrix <- function(x = matrix()) {
      # intialize the inverse matrix to NULL
      i <- NULL
      # set a new matrix if changed after original
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      # retrieve the currently cached matrix
      get <- function() x
      # cache the inverse matrix. Does not compute the inverse.
      setinverse <- function(inverse) i <<- inverse
      # retrieve the cached inverse matrix
      getinverse <- function() i
      # return a list of functions to set and get the cached matrix and its inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Purpose of cacheSolve: To compute and store the inverse of a matrix stored using "makeCacheMatrix"
## Parameter: a list of functions $set, $get, $setinvserse, and $getinverse, established by  
##    calling MakeCacheMatrix before calling cacheSolve
## Retured value: 
##    a matrix representing the inverse of the matrix that was cached using "makeCacheMatrix". 
##    If the original matrix has not been changed since the inverse was computed, the cached 
##    value is returned, without having to recompute the inverse of the original matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)  #return the cached inverse matrix
      }
      data <- x$get()  #retrieve the cached matrix
      i <- solve(data, ...)  #compute the inverse of the cached matrix
      x$setinverse(i)  #cache the computed inverse of the cached matrix
      i      #return the computed inverse of the cached matrix
}
