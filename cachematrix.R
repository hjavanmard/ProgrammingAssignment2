## Put comments here that give an overall description of what your
## functions do

## the first function provides a list of four functions to cache the recent matrix, printing 
## it, cache the Inverse of matrix and printing it using set,get,setInv, and getInv functions
## respectively.

makeCacheMatrix <- function(x = matrix()) {
		inv<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function() x
    setInv<-function(z) inv<<-z
    getInv<-function() inv
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## This function just checks if the matrix x has been inversed or not. If we cahed the inverse
## before then we just return it otherwise we calculate it and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    print("This matrix has been already inversed")
    return(inv)
  }
  inv<-solve(x)
  x$setInv(inv)
  inv
}
