## makeCacheMatix creates a special "vector", 
## which is really a list containing a function to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse of the matrix
##  4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv.matrix <- NULL
    set <- function(y){
        x <<- y
        inv.matrix <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv.matrix <<- inverse
    getinv <- function() inv.matrix
    list( set=set, get=get,setinv = setinv,getinv=getinv)
}


## cacheSolve checks to see if the inverse of the matrix has been
## previously calculated, and if so returns it.  If not, calculates
## and stores and returns the inverse of the matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv.matrix <- x$getinv()
    if(!is.null(inv.matrix)) {
        message("getting cached data")
        return(inv.matrix)
    }
    data <- x$get()
    inv.matrix <- solve(data, ...)
    x$setinv(inv.matrix)
    inv.matrix
}
