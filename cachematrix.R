## Functions makeCacheMatrix and cacheSolve accept a inversible ##matrix and inverses it and when called (cacheSolve)for the 2nd ##time returns the inverse from cache.

## This function takes a matrix as its argument and returns a ##list of functions. The functions 
## set - sets the value of the matrix
## get - gets the value of the matrix
## setinv - sets the inverse of the matrix
## getinv - gets inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	   inv <- NULL

## if the matrix is changed by the user in set function, inverse ## of the matrix is set to NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        setinv <- function(inv1) inv <<- inv1

        getinv <- function() inv

## a list of functions is returned back in the below statement

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##  This function calculates the inverse of a matrix 
##If it is in the cache, inverse from the cache along with the message "getting cached data" is displayed.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()

        ## check if the inverse is already calculated

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        matrix <- x$get()

        ## Inverse the matrix using solve function
        inv1 <- solve(matrix)
        x$setinv(inv1)

	   ## Inverse matrix is returned
        inv1
}