## ProgrammingAssignment2: Caching the Inverse of a Matrix
## 

## this function will compute the inverse of a given matrix and cache it

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  # set the content of the matrix
                x <<- y         # enables redefining the variables through...
                m <<- NULL      # ... parent environments
        } 
        get <- function() x # get the content of the matrix
        setmatrix <- function(solve) m <<- solve # compute the inverse
        getmatrix <- function() m # get the inverse
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## this function will compute and output 
## the inverse of a given matrix
## the function skips the computing, if the inverse has already been cached

cacheSolve <- function(x, ...) {
        m <- x$getmatrix() # get the inverse
        if(!is.null(m)) {  # if the inverse is cached, the function will output it 
                message("getting cached data")
                return(m)
        }       
        matrix <- x$get()  # if not, it will get the inputmatrix and ...
        m <- solve(matrix, ...) # ... compute the inverse again
        x$setmatrix(m)
        m                 # return a matrix that is the inverse of 'x'
}