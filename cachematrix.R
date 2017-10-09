## These two functions below enable the caching of the inverse of a matrix in order to reduce repeated computation, making code faster
## and more efficient. 

## The MakeCacheMatrix function will set up a matrix 'x' with the following four functions in its environment: set, get, setinverse, 
## and getinverse. It is best to place the result of this function call in a new matrix, i.e. b <- makeCacheMatrix(a), where 
## a <- matrix(1:10, nrow = 2, ncol = 5). Then, the $ operator will will work with all four functions. These four functions will 
## interact with the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
    
       ## set inverse to a blank matrix    
       inverse <- matrix(nrow = 1, ncol = 1)                 
 
       ## sets the matrix x to y if use set() function
       set <- function(y)  {
        x <<- y
        inverse <<- matrix()
       }  
       
       ## gets the matrix x
       get <- function() x  
       
       ## can set inverse to z
       setinverse <- function(z = matrix())  inverse <<- z  
       
       getinverse <- function() inverse  
    
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## CacheSolve will calculate the inverse matrix and set/retrieve it to/from the parent environment, and will return a matrix that 
## is the inverse of 'x'.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.na(inverse[1,1])) {message("getting cached data")}
          else {inverse <- solve(x$get()) 
          x$setinverse(inverse)
          }
      inverse
}

## Here is a sample of code that can be used with these two functions:
## a <- matrix(c(1,2,7,6,5,4,3,9,7), nrow=3, ncol=3)
## b <- makeCacheMatrix(a)
## cacheSolve(b)
## cacheSolve(b) - second time will show "getting cached data"
