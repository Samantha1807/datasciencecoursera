## The pair of functions cache the inverse of a matrix


## This function will set the value of the matrix, get the value of the matrix
## set the value of the inverse and get the value of the inverse
## It creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
                x <<- y
                inv <<- NULL
          }
          get <- function() x
          setInv <- function(inverse) inv <<- inverse
          getInv <- function() inv
          list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The inverse of the "matrix created in the above function is 
## computed with this function. If it has already been computed 
## then it retrieves it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv ()
        if(!is.null(inv)){
          message("Getting cached data")
          return(inv)
        }
        mat_in <- x$get()
        inv<- solve(mat_in, ...)
        x$setInv(inv)
        inv
}
