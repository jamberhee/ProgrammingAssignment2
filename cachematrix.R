## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #creates special matrix
  i <- NULL 
  set <- function(y) { #sets matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x #gets matrix
  setinverse <- function(inverse) i <<- inverse #sets inverse
  getinverse <- function() i #gets inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) { #checks to see if inverse has been calculated
    message("getting cached data")
    return(i) #gets mean from cache and skips computation
  }
  data <- x$get()
  i <- solve(data, ...) #calculates inverse
  x$setinverse(i) #sets inverse in cache
  i
}
