makeCacheMatrix <- function(x = matrix()) {
        ## sets im(Inverse Matrix) to NULL
        im <- NULL 
        ## 1) set value of matrix
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        ## 2) get value of matrix
        get <- function() x
        ## 3) set value of im
        setsolve <- function(solve) im <<- solve
        ## 4) get value of im
        getsolve <- function() im
        
        ## Create new object list where the '$-extract operator'can be used 
        ## to access the functions name later on
        list(set = set, get=get, setsolve = setsolve,
            getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        ## retreive input of 'getsolve' and puts it in im.
        im <- x$getsolve()
        ## checks if im has already been calculated. if so, it will return im.
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        ## if not, store new matrix into data.
        data <- x$get()
        ##calculate new inverse matrix and store it in im
        im <- solve(data)
        ## set new calculated in parent environment.
        x$setsolve(im)
        ## print im
        im
}