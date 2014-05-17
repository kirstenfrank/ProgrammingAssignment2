## Program started 5/15/2014 by Kirsten Frank. Edited 5/16/2014
## makeCacheMatrix takes a matrix and defines four methods (functions)
## these four functions are returned as a list.

makeCacheMatrix <- function(x = matrix()) {
    cache<<-NULL   #prepare an object to be used as the cache
    set<-function(y) {
        x<<-y
        cache<<-NULL
    }
    ## set moves y to x and sets the cache to NULL
    get<-function() x
    setinverse <- function(inverse) cache <<- inverse
    getinverse <- function() cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    ## this list of functions is the output of the function. Save it to some object.
}


## The cacheSolve funtion uses the functions defined in makeCacheMatrix to 
## find a cached inverse if it exists and calculate it if it doesn't exist.

cacheSolve <- function(x, ...) {
    cache <- x$getinverse()
    if(!is.null(cache)) {
        ## if there is a value in the cache, this returns the value.
        message("getting cached data")
        return(cache)
    }
    else {
        ## the else code gets the matrix and calculates the inverse of it.
        data <- x$get()
        ## this puts the value of the matrix into data.
        cache <- solve(data)
        ## this inverts the matrix from data and puts it into cache.
        x$setinverse(cache)
        cache
    }    ## Return a matrix that is the inverse of 'x'
}
