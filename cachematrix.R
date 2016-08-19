## The function makeCacheMatrix() will build a set of functions and 
##returns the functions within a list to the parent environment
##It stores these functions within an environment defined by the function.
##It also stores the objects x and inv
##The functions are stored as a list object
##cacheSolve makes use of lexical scoping to 
##take an argument that is returned by makeCacheMatrix() 
##and retrieve the matrix inverse from the cached value 
##that is stored in the makeCacheMatrix() object's environment.

##This makes four functions: set(),get(),setinv(), and getinv(); and the objects x and inv
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

myMatrix<-makeCacheMatrix(matrix(c(1,.3,.3,1),2,2))


## This function returns the matrix inverse from the cached matrix
##object myMatrix that is stored in the makeCacheMatrix() object's environment.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}

cacheSolve(myMatrix)

