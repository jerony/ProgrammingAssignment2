## MakeCacheMatrix and cacheSolve are a pair of fuctions that interact in a way 
## that allows you to store and recall the inverse of a matrix, (and reset this
## stored inverse if the underlying matrix changes).

## MakeCacheMatrix creates an object that stores the current value of the inverse

MakeCacheMatrix <- function(x = matrix()) {
		t <- NULL
            retrm <- function() {print(x)}
            setinv <- function(inverse){t <<- inverse}
            retrinv <- function() {t}
		structure(list(retrm=retrm,setinv=setinv,retrinv=retrinv,filler=NULL),dim=c(2,2))
                          }

## cacheSolve retrieves the inverse stored in the body of MakeCacheMatrix if
## it is not a null value; if it is a null value, this function calculates the 
## inverse of the matrix, then stores it in the body of MakeCacheMatrix.

cacheSolve <- function(m,...) {
		n <- m$retrinv()
	      if(!is.null(n)) {
			message("retrieve stored inverse")
                  return(n) }
                  else {
				  q <- m$retrm()
                          w <- solve(q,...)
                          z <- m$setinv(w)
                          w }
                 }
