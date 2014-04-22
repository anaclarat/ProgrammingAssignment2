## These functions acts as a group that takes a matrix and calculates its 
#inverse, caching its value, thus,if a 2nd call is made to the function, 
#returns its value without recalculating it, optimizing computational cost. 
#It also optimizes memory, since the object instantiated can be resetted to 
#a new matrix through the 'makeCacheMatrix$set()' function  and running 
#the 'cacheSolve()' function again.


### The 'makeCacheMatrix' function creates a list of values:

##Set - which is the function that provides the ability to reset the 
##object to a new value:
#function(y){ 
#  x <<- y
#  inv <<- NULL 
#}

##Get - which is the function that returns the current matrix in memory:
#function() x

##Set_inv - which is the function that will return null forcing execution of 
##"inv <- solve(data)" in cacheSolve which calculates and returns the inverse. 
##If user tries to call it without arguments, null is default to return null and 
##avoid wrong results.
#function(s=NULL) inv <<- s

##Get_inv - which is the function that returns the current inverse matrix 
##in memory. This is done by getting its value from the 
##'cacheSolve' function in line: 'inv <- solve(data)':
#function() inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){ 
    x <<- y
    inv <<- NULL 
  }
  
  get <- function() x 
  set_inv <- function(s=NULL) inv <<- s 
  get_inv <- function() inv 
  
  list(set=set, get = get,
       set_inv = set_inv,
       get_inv=get_inv)
}

## The cacheSolve function returns the inverse of a given matrix.There is no 
##recalculation if a 2nd or 3rd call is made to the function, since it uses the 
##'makeCacheMatrix' function to stores the instantiated object and calculation 
##results. This is done by testing if there is already a result in the 'inv' 
##variable, and , if so, returning it.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)){
    message('getting chached data')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inv(inv)
  inv
}