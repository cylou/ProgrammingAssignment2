## Put comments here that give an overall description of what your
## functions do --> comments are in the functions for more clarity

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## before any computation, the inverse is set to NULL
        set <- function(y) {
                x <<- y ## we replace the old matrix by the new one
                inv <<- NULL   ## by setting a new matrix we want to get ride of the old result
        }
        get <- function() x ## this function returns the matrix defined
        setinv <- function(i) inv <<- i ## this function only stores its argument into the variable 'inv'
        getinv <- function() inv ## this function returns the variable 'inv' set by the function above
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
## a list of function is created with the 4 functions above
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv() ## we store into inv the result of the function getinv defined above
        if(!is.null(inv)) { ## if the inverse as already been calculated, we take the value stored in cache
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ## we use the getter defined in makeCacheMatrix to store the matrix in 'data'
        inv <- solve(data, ...) ## we calculate the inverse of the matrix with the function 'solve()' and store the result in 'inv'
        x$setinv(inv) ## we use the setter defined in makeCacheMatrix to set the computed inverse
        inv ## we return the value of 'inv'

}
