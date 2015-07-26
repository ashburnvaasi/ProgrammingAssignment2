## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
# 
## This is built based on the vector mean example provided
#The first function, makeVector creates a special "vector", which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
# get the value of the mean
#

## Function makeCacheMatrix () 
## The function makeCacheMatrix creates a matrix and defines the following functions
## 1. set the value for matrix
## 2. get the matrix value
## 3. set the value of matrix inverse (store it for later retrieval)
## 4. getinverse ---> to obtain inverse of the input matrix


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	set <- function(y) {
	    x <<- y
	    inv <<- NULL
	}
	get <- function() x 
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function returns the cached vs computed inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        ## Debug statement, need to be commented before submission
        print("Obtain value from cache")
        return (inv)
    }
    
    data <- x$get()
    
    inv <- solve(data)
    
    x$setinverse(inv)
    ## final return
    inv
}

## Execution

## > my_l1 <- c(1:2)
## > my_l2 <- c(2:3)
## > my_td <- cbind(my_l1, my_l2)
## > class(my_td)
## [1] "matrix"

## > m <- makeCacheMatrix(my_td)
## > m$get()
## my_l1 my_l2
## [1,]     1     2
## [2,]     2     3
## > cacheSolve(m)
## [,1] [,2]
## my_l1   -3    2
## my_l2    2   -1
## > cacheSolve(m)
## [1] "Obtain value from cache"
#[,1] [,2]
##my_l1   -3    2
##my_l2    2   -1

