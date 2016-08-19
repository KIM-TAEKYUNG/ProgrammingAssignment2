
makeCacheMatrix <- function(inputVal=matrix()){
   var_inverse <- NULL
   set <- function(setVal) {
          inputVal <- setVal
          var_inverse <<- NULL
    }
   get <- function() inputVal
   setInverse <- function(invVal) var_inverse <<- invVal
   getInverse <- function() var_inverse
   list(set=set,get=get, setInverse= setInverse, getInverse= getInverse)
}

cacheSolve <- function(x, ...){
   var_inverse <- x$getInverse()
   if( !is.null(var_inverse) ){
     message("getting cached data !!!")
     return(var_inverse)
   }
  var_tmp <- x$get()
  var_inverse <- solve(var_tmp, ...)
  x$setInverse(var_inverse)
  var_inverse
}
