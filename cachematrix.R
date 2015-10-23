## Make a Cache Matrix from a Matrix
## Cache Matrix consist of a Matrix & an Inverse Matrix
## with Set & Get functions for both.
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize funtion by set m to NULL 
    m <- NULL
    
    ## Set Matrix function
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## Get Matrix function
    get <- function() x
    
    ## Set Inverse Matrix function
    setinverse <- function(inverse) m <<- inverse
    
    ## Get Inverse Matrix function
    getinverse <- function() m

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Take a Cache Matrix parameter 
## If there is already an Inverse Matrix cached in parameter, just return it  
## Else solve an Inverse Matrix, set into the paramter and return the Inverse Matrix 
cacheSolve <- function(x, ...) {
    ## Set m as inverse matrix get from parameter
    m <- x$getinverse()
    
    ## Return cached Inverse Matrix
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## Get Matrix from parameter & solve a inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    
    ## Set the Inverse Matrix into parameter & return
    x$setinverse(m)
    m
}
