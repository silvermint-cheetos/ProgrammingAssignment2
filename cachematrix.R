## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## cached inverse of matrix
        inv <- NULL
        
        ## getter/setter for matrix
        set <- function(y){
                y <<- x
                inv <<- NULL
        }
        get <- function() x
        
        ## getter/setter for matrix inverse
        setinv <- function(inverse) inv <<-inverse
        getinv <- function() inv
        
        ## assign each of these functions as an element within a list(), and returns it to the parent environment.
        list(set=set, get=get, setinv=setinv, getinv=getinv)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        ## return cached matrix inverse if it's been already computed
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## compute inverse of matrix
        data <- x$get()
        inv <- solve(data, ...)
        
        ## cache inverse matrix
        x$setinv(inv)
        
        ## return inverse matrix
        inv
}
