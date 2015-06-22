## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The MakeCacheMatrix function creates and provides set and get functions for a matrix i

makeCacheMatrix <- function(matrixCache = matrix()) {
     
   
     set <- function(y) {
          # set the set the matrixCache in the global environment and null the inverse chache, 
          # also in the global environment
          staticMatrixCache <<- y)
          inverseMatrixCache <<- NULL
     }
     # pass back matrix
     get <- function() staticMatrixCache
     
     # use R, solve function to calculate the invers of the matirx in matrix 
     setInverse <-  function(solve) inverseMatrixCache <<- solve
     
     getInverse <- function() inverseMatrixCache

     list(set = set, get = get,
          setInverse  = setInverse ,
          getInverse  = getInverse )
     

}


## Write a short comment describing this function

cacheSolve <- function(x, f) {
        ## Return a matrix that is the inverse of 'x'
     # initialize return variable as an empty matrix
     ##browser()
     ret <- NULL
     ccc <- f()$get()
     bbb <- x
     if(!identical(bbb, ccc)){
          
          f()$set(bbb)

          inverseMatrixCache <- solve(x)
         
          
     }else{
          if (is.null(inverseMatrixCache)) {
               f()$setInverse( solve(x))
          }
          ret <- f()$getInverse()
     }
     
     ret
}



