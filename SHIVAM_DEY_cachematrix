makeCacheMatrix <- function() {
  sam <- NULL
  inv_sam <- NULL
  set <- function(x){
    sam <<- x
    inv_sam <<- NULL
  }
  get_inv <- function(){
    if (is.null(inv_sam)){
      inv_sam <<- solve(sam)
      }
    inv_sam
    }
  list(set = set, get_inv = get_inv)
  }



  
cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
