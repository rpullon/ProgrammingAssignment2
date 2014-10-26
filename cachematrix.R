## These functions store a matrix and calculates the inverse.

## makeCacheMatrix takes a matrix as an input and contains a list of four functions which allow you to set a new matrix, get the matrix from memory, set the inverse of the matrix, or retreive the inverse from memory.

makeCacheMatrix <- function(matr = matrix()) {
    
    matrixinverse <- NULL ##set inverse to NULL
    
    ## $set sets a new value for the matrix
    set <- function(y){ 
        matr <<- y
        matrixinverse <<- NULL
    }
    ## $get prints the matrix to the dashboard
    get <- function() {
        matr
    }
    ## set the inverse of the matrix
    setinverse <- function(inverse) {
        matrixinverse <<- inverse
    }
    ## print the inverse to the dashboard
    getinverse <- function() {
        matrixinverse
    }
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes the output of makeCacheMatrix (list of four functions). The function then retreives the matrix inverse from memory if it exists, otherwise it computes the inverse.

cacheSolve <- function(x,...){
    ## get current inverse
    matr <- x$get()
    matrixinverse <- x$getinverse()
    
    ## if it is not null, get from memory
    if(!is.null(matrixinverse)){
        ## Make sure same size
        dimMatrix <- dim(matr)
        dimInverse <- dim(matrixinverse)
        same <- identical(dimMatrix,dimInverse)
        
        ## If same size, assume same matrix, get cached data
        if(same) {
            message("getting cached data")
            return(matrixinverse)
        }
    }
    
    ## otherwise work out matrix inverse
    matrixinverse <- solve(matr)
    
    x$setinverse(matrixinverse)
    matrixinverse ## print out to consol
}
