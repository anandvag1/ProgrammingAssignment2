## A function to create a special vector of matrix function and another function
## to get the inverse from the special vector and if no inverse calcialte the inverse

## A function to create a special vector of matrix function

makeCacheMatrix <- function(x = matrix()) {
  
  iMatrix <- NULL
  
  setM <- function(y){
    x <<- y
    iMatrix <<- NULL
  }
  
  getM <- function() x
  
  getIMatrix <- function() iMatrix
  
  setIMatrix <- function(im) iMatrix <<- im
  
  list(setM = setM, getM = getM,
       setIMatrix = setIMatrix,
       getIMatrix = getIMatrix)

}


## Function to get the inverse from the special vector and if no inverse calcialte the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getIMatrix()
  
  if (!is.null(iMatrix)){
    message("getting cached data")
    return(iMatrix)
  }
  
  mat <- x$getM()
  
  iMatrix <- solve(mat)
  x$setIMatrix(iMatrix)
  iMatrix
  
}
