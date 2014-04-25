## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse

## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache



makeCacheMatrix <- function(x = matrix()) {
       m <- NULL                                         ## initialise to NULL so it is easy to test
       set <- function(y) {                             ## set the value of the matrix where y is x 
              x <<- y
              m <<- NULL                                ## and m is still null
       }
       get <- function() x                              ## matrix data can be retrieved after it is set 
       setinverse <- function(inverse) m <<- inverse    ## m is no longer null when the inverse matrix is calculated
       getinverse <- function() m                       ## the inverse of the matrix can be retrieved once it is calculated
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x

cacheSolve <- function(x, ...) {
       m <- x$getinverse()                       ## if getinverse() has not been calculated, returns NULL
       if(is.null(m)) {                          ## therefore we need to calculate it
              data <- x$get()                    ## retrieves the special matrix
              m=solve(data)                      ## calculate the inverse (special case of solve())
              x$setinverse(m)                    ## stores the inverse matrix in x
              m                                  ## prints the inversed matrix
       }
       else {                                    ## inversed matrix has already been calculated
              message("getting cached data")
              return(m)                          ## return inversed matrix
       }
}
