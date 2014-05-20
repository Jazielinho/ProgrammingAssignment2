#Example usage:
#x <- matrix(c(1,2,3,4),nrow=2,ncol=2)    	#Create a squared matrix x
#cx <- makeCacheMatrix(x)                  #Create our special matrix
#cx$get()                                  #Return the matrix
#cacheSolve(cx)                          	#Return the inverse
#cacheSolve(cx)                            #Call the 2nd time, so return


#####################################################################################
#Function makeCacheMatrix: return a list of function:
	#-set the value of the matrix
	#-get the value of the matrix
	#-set the value of the inverse
	#-get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {

	  #Let's check if we have correct input
	    if (!is.matrix(x)) {
   		stop("Please give a matrix")
  		}
		np<-dim(x)
		n<-np[1]
		p<-np[2]

	    if (n!=p) {
   		stop("Please give a squared matrix")
  		}
	#################################################
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        		}
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#####################################################################################
# Function cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


#####################################################################################
