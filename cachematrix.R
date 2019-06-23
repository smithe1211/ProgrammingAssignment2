## These functions will cache the mean values into a matrix
## and store the inverse of the matrix

## This function will cache a special matrix of mean values

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setmean <- function(mean) i <<- mean
        getmean <- function() i
        list(set = set, get = get, setmean = setmean, getmean = getmean)
}





## this function checks if the inverse has already been calculated and returns that
## or calculates the inverse and returns that value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getmean()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setmean(i)
        i
}
