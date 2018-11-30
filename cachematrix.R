## Put comments here that give an overall description of what your
## The functions below aim to solve the inverse of a matrix, but store the inverse
## value once it is calculated so it can be referenced back to if called upon, rather than 
## recalculating it every time it is need.

## The makecachematrix function creates a list of 4 functions that allow us to retrieve 
## the inverse value of a matrix that is calculated if it exists.

makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
     
}


## The cacheSolve function takes a matrix and aims to return the inverse matrix of that matrix.
## To act as efficiently as possible, the function first uses functions stored in makeCacheMatrix
## to assess whether the inverse has been calculated already. If it has, the funtion will take that 
## value, if it has not, the function will calculate the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}

## Below I am testing to see if the functions work as intended.

x<-matrix(1:4,2,2)

r<-makeCacheMatrix(x)

w<-cacheSolve(r)
