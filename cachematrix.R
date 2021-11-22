## L. Hoadley R Programming Assignment 2
## November 21, 2021

## These functions work in tandem to: 
## 1. compute a matrix inversion when no cached matrix inversion is available or 
## 2. used cached matrix inversion to save computation time 
## rgument, x, returned by makeCachematrix()

## TEST CODE For PEER REVIEWERS
## x<-makeCacheMatrix(matrix(c(4,2,7,6),nrow=2, ncol=2))
## cacheSolve(x)
## cacheSolve(x)

## The code above should return the following inverse
##[,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
##the second run of cachSolve(x) should return the same inverse with the
##message "getting cached data"

## This first function creates takes matrix x as an argument and creates 
## a list with four functions:
##1. set() sets the value of the matrix
##2. get() retrieves the value of the matrix 
##3. setinv() sets the value of the matrix inversion
##4. getinv() retrieves the value of the matrix inversion
## When using the makeCacheMatrix function you must set it equal to an object x
## in order to get the expected output from cacheSolve (see test code above)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setinv <- function(inv) {m <<- inv}
    getinv <- function() {m}
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This second function retrieves a cached matrix inversion if it is available
## for the matrix. Otherwise, it calculates a matrix inversion. You must 
## first set pass a matrix argument through makeCacheMatrix and set 
## makeCacheMatrix equal to object x before using cacheSolve (see example code above)

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}