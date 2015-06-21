## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    validate_matrix  <- function(x) {
        if (  nrow(x) != 2 || ncol(x) != 2 )
        {
            message("Error:matrix not 2X2")
            x <- NULL
            return FALSE
        } 
        x
    }
    setmatrix_inv <- function(y) {
        x <<- y
        m <<- solve(x )
    }
    getmatrix <- function() { validate_matrix(x)}
    getmatrix_inv <- function() m
    list( getmatrix = getmatrix,
         setmatrix_inv = setmatrix_inv, 
         getmatrix_inv = getmatrix_inv,
         validate_matrix = validate_matrix)     
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getmatrix()
    if(is.null(data)) {
        message("Error getting matrix inverse")
          
    } else {
        m <- x$setmatrix_inv( data )
        m
    }
}

##
## To test 
## a<- makeCacheMatrix( matrix(1:6,3,3))
## cacheSolve(a)
##
## a<- makeCacheMatrix( matrix(1:4,2,2))
## cacheSolve(a)
## cacheSolve(a)
##
##
