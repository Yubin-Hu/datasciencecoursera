## The makeCacheMatrix and cacheSolve functions could 
## calculate and cache the inverse of a matrix.


## The makeCacheMatrix function creates a special "matrix" object 
## which is actually a list including the following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## which is returned by makeCacheMatrix above. 
## It first checks whether the inverse of the matrix has been calculated
## and whether the matrix has been changed.
## If the inverse has been calculated and the matrix is the same,
## it skips calculation and retrieves the inverse from the cache.
## Otherwise, it calculates the inverse of the data, sets the value
## of the inverse in the cache via the setinv function and sets the 
## value of the new matrix in the cache via the set function.

cacheSolve <- function(x, y = matrix(), ...) {
## cacheSolve function has two arguments, 'x' is the special "matrix" returned
## by makeCacheMatrix function, 'y' is the original matrix or a different one.
        inv <- x$getinv()
        temp <- x$get()
        if(!is.null(inv) && y == temp) {
                message("getting cached data")
                return(inv)     ## Retrieve value of the inverse from the cache
        }
        x$set(y)                ## Set the value of the new matrix 'y' in the cache
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)           ## Set the value of the inverse in the cache
        inv                     ## Return a matrix that is the inverse of 'y'
}



