
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## xinvert was initialized to a matrix of NA with dimensions of matrix x
## set() function changes the values of matrix x to another matrix y.
## get() returns the values of matrix x
## setinvert() changes the values of inverted matrix xinvert to set values of invertmatrix
## getinvert() returns the values of inverted matrix xinvert

makeCacheMatrix <- function(x = matrix()) {
    xinvert <- matrix(data =NA, nrow = dim(x)[1], ncol = dim(x)[2])
    set <- function(y) {
        x <<- y
        xinvert <- matrix(data =NA, nrow = dim(x)[1], ncol = dim(x)[2])
    }
    get <- function() 
    {
        return(x)
        
    }
    setinvert <- function(invertmatrix) 
    {
        xinvert <<- invertmatrix
    }    
    getinvert <- function() 
    {
        return(xinvert)
        
    }
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## Write a short comment describing this function
## xinvert2 returns the inverted matrix stored in x(x is a list of four functions and values of x and xinvert)
## check whether the number of rows and number of columns in inverted matrix are equal. If not, throw error message
## check whether all numbers of inverted matrix are NA (whether they have been calculated or not). If any number is not NA, it means
## these values stored previously calculated inverted matrix and we should use it directly
## otherwise call x$get() to get the original matrix x, then use function solve() to obtain the inverted matrix
## The final step is to stored the newly inverted matrix xinvert2 to x and returned xinvert2

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinvert2 <- x$getinvert()
    if (dim(xinvert2)[1] != dim(xinvert2)[2])
    {
        stop("Not square matrix!")
    }
    for (i in 1:length(xinvert2))
    {
        if (!is.na(xinvert2[i]))
        {
            return(xinvert2)
        }
        
    }
    xdata <- x$get()
    xinvert2 <- solve(xdata, ...)
    x$setinvert(xinvert2)
    return(xinvert2)
}
