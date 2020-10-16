## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        Inv<- NULL
        ## the set function designed to set the value of the matrix
        set <- function(y)
        {
                x<<- y
                Inv<<- NULL
        }
        ## The get function that returns the matrix
        get <- function() x
        ## The setter function of the inverse
        setInverse <- function(inverse) Inv<<- inverse
        ## the get function that returns the inverse
        getInverse <- function() Inv
        ## Return a list of the functions function
        list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## in this case, it is Inv
        ## first we get the inverse of x
        Inv <-x$getInverse()
        ## if the inverse is null, we get the cache data and return the Inv variable
        if(!is.null(Inv))
        {
                message("get cache data")
                return(Inv)
        }
        ## if the Inv of the matrix is not null, we get the matrix itself
        data <- x$get()
        ## use solve() to calculate the inverse
        Inv <- solve(data, ...)
        ## set the inverse 
        x$setInverse(Inv)
        ## then return it
        Inv
}
