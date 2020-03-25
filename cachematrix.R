# This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
#set value of matrix
  i <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
#get value of matrix
  get <- function() x
#set inverse of matrix
  setinverse <- function(inverse) i <<- inverse
#get inverse of matrix
  getinverse <- function() i
#create list
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


# This function computes the inverse of the special matrix returned by previous function 
# If the inverse has already been calculated and the matrix is unchanged 
# then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
#checks if inverse has already been calculated
  i <- x$getinverse()
  if(!is.null(i)) {
#lets the user know if the inverse has already been calculated and cached
    message("getting cached data")
    return(i)
  }
#otherwise it calculates the inverse of the data
  data <- x$get()
  i <- inverse(data, ...)
#function sets the value of the inverse in the cache
  x$setinverse(i)
  i
}
