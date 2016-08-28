## the following functions are used to cache the inverse of a matrix

## The first function, makeCachematrix creates a special "Matrix", 
## which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of inverse of the matrix 
## 4.get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y){
    
    x <<- y
    i <<- NULL
  }

  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)

  }

##The second function calculates the inverse of
##the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already
##been calculated. If so, it gets the inverse from the cache
##and skips the computation. Otherwise, it calculates the
##inverse and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
        }
