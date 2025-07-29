# sam and inv_sam act as storage that hold the matrix and its inverse, respectively.
# set(x) assigns a new matrix to sam (overwriting previous content, similar to repointing a pointer), and resets inv_sam to NULL (the cache is cleared, as the old inverse is no longer valid).
# get_inv() first checks if inv_sam is NULL. If it is, it computes the inverse using solve(sam) and stores it in inv_sam.
# The return value is a list containing the set and get_inv functions, enabling object-like ("pointer-like") access to the underlying matrix and its inverse cache.

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


# cacheSolve, operates like a pointer-driven cache accessor for an invertible matrix built using a "matrix object" (from makeCacheMatrix)
# x$getInverse() acts like dereferencing a pointer to a cached inverse. If the "pointer" already holds the computed inverse (not NULL), it is returned immediately with a message (no recalculation needed).
# If the pointer is NULL (no cached data), x$get() dereferences the matrix object to obtain the underlying matrix value.
# solve(mat, ...) computes the matrix inverse (an expensive operation), producing a new object.
# x$setInverse(j) "repoints" the inverse’s pointer in the object to this newly computed inverse, updating the cache.
# Finally, the function returns the inverse pointer's value so that future calls use this cached result.
  
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
