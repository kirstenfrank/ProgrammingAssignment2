## This function takes a vector, and defaults to an empty numberic vector if none is 
## supplied. The output is a list of functions (get, set, getmean, setmean).
## you must use those functions to get useful output. James A. Stephanson posted 
## a good explanation "Here's how makeVector and cacheMean work"
## that I used to understand the points here. I also used 
## the "some fun with Assignment 2's concepts" by Richard Ambler.
## Kirsten Frank Started on 5/16/2014.

makeVector <- function(x = numeric()) {
    ## Sets m (the target of the mean) to NULL
    m <- NULL
    ## set is a function that takes a vector and assigns it to x, and passes it to be
    ## used by makeVector. It also sets m to NULL. This means that the mean has not 
    ## been calculated yet.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ## get is a function that takes no arguments and returns x.
    setmean <- function(mean) m <<- mean
    ## setmean is a function that takes a function, and uses it on ? and returns the 
    ## value to m.
    getmean <- function() m
    ## getmean returns m.
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
    ## this list of functions is the output of the function. Save it to some object.
}

cachemean <- function(x, ...) {
    ## x has to be the result of makeVector. Therefore, x is a list that contains
    ## 4 functions (set, get, setmean, getmean)
    ## I guess the ... allows you to pass parameters that mean can use like
    ## na.rm = TRUE
    m <- x$getmean()
    if(!is.null(m)) {
        ## if there is a value in m, this returns the value.
        message("getting cached data")
        return(m)
    }
    else {
        ## this gets the vector and calculates the mean of it.
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
    }
}
