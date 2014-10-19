## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      #initial value for the inverse of matrix x is null
      inv <- NULL
      #set matrix value, also assign NULL to the inverse in global environment
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      #get function retrive matrix x
      get <- function()x
      #cache calculated inverse into global environment
      setinverse <- function(solve)inv <<- solve
      #retrive cached inverse 
      getinverse <- function()inv
      #set function calls
      list(set=set, get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      #first try to retive if inverse already exist in cache
      inv <- x$getinverse()
      #if inverse already exist in cache, retive its value from global environment
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      #set data by calling get function
      data <- x$get()
      #solve for inverse
      inv <- solve(data, ...)
      #assign inverse value to function call setinverse 
      x$setinverse(inv)
      #return inverse value
      inv
}

