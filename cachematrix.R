## The following function work together to create a 
## square invertible matrix and make the inverse of the matrix

## makeCacheMatrix creates and return a list of functions
## create the matrix -> get the value of matrix -> 
## invert the matrix and store in inv -> get the inverted matrix from inv
## return the created functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve is used to get or set the inverted matrix
## and calculates the inverse of the matrix created in makeCacheMatrix
## return inverted matrix from inv if it existed
## else create the matrix data and store it's inverted value

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinv(inv)
        inv	
        ## Return a matrix that is the inverse of 'x'
}

## sample run
## > x <- makeCacheMatrix()
## > x$set(matrix(1:4, 2, 2))
## > cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(x)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5