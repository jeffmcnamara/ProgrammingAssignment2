## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function will Cache a matrix

## makeCachematrix creates a special matrix and caches the matrix 
## and caches the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        ##Intialize two objects x and s; x is a empty matirix s is 
        ##set to Null 
        s <- NULL
        ## defines set funtion
        set <- function(y) {
                ## sets cache x as y
                x <<- y
                ##sets s value as null, clears s in parent environment 
                s <<- NULL
        }
        ## defines getter for matrix x 
        get <- function() x
        ## setsolve sets the solve to get inverse of the matrix
        setsolve <- function(solve) s <<- solve
        ## getsolve sets getter for inverse
        getsolve <- function() s
        #assignd vaules and returns the values to parent environment
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## Write a short comment describing this function
##cacheSolve is a function that eithers returns the inverse calculated in the 
## makeCachematrix or calculates the new inverse of the a matrix.  It will rerun 
## the message 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##calls inverse from makeCachematrix
        s <- x$getsolve()
        ## check to see if it is null; if false gets it from cache 
        ##if true calculates new inverse
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s
} 
