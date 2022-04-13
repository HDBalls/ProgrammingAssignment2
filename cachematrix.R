## There are two functions here:
##(1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##(2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## The makeCacheMatrix function allows for the setting and getting the values of the matrix as well as the inverse.
## The major assumption here is that the matrix supplied is always invertible.

makeCacheMatrix <- function( x = matrix() ) {
    i <- NULL
    set_matrix <- function( y ) {
            x <<- y
            i <<- NULL
    }
    get_matrix <- function() x

    set_matrix_inverse <- function(inverse) n <<- inverse

    get_matrix_inverse <- function() i

    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_matrix_inverse = set_matrix_inverse,
         get_matrix_inverse = get_matrix_inverse)
}



## The following function calculates the inverse of the result created with the initial function.
## The first step is to see if the matrix has already been cached. If true, it gets the value from the
## 	cache then it does not compute. If false, it calculates.
cacheSolve <- function(x, ...) {
    i <- x$get_matrix_inverse()
    if( !is.null(i) ) {
            message("getting cached data")
            return(i)
    }
    data <- x$get_matrix()
    #i <- solve(data) %*% data
	i <- solve(data, ...)
    x$set_matrix_inverse(i)
    i
}
