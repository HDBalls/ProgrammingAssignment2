## There are two functions here:
##(1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##(2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## The makeCacheMatrix function allows for the setting and getting the values of the matrix as well as the inverse.
## The major assumption here is that the matrix supplied is always invertible.

makeCacheMatrix <- function( m = matrix() ) {
    n <- NULL
    set_matrix <- function( o ) {
            m <<- o
			#m <<- matrix
            n <<- NULL
    }
    get_matrix <- function() {
    	m
    }
    set_matrix_inverse <- function(inverse) {
        n <<- inverse
    }
    get_matrix_inverse <- function() {
        n
    }
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_matrix_inverse = set_matrix_inverse,
         get_matrix_inverse = get_matrix_inverse)
}


## The following function calculates the inverse of the result created with the initial function.
## The first step is to see if the matrix has already been cached. If true, it gets the value from the
## 	cache then it does not compute. If false, it calculates.
cacheSolve <- function(x, ...) {
    m <- x$get_matrix_inverse()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    data <- x$get_matrix()
    m <- solve(data) %*% data
    x$set_matrix_inverse(m)
    m
}
