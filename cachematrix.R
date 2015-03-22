makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
1) set the value of the matrix (set)
2) get the value of the matrix (get)
3) set the value of the matrixinverse (setinverse)
4) get the value of the matrixinverse (getinverse)	

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
	# inverse to null is initialized

  set <- function(y) {
  	# set: new value of a matrix is set

    x <<- y
    # value is stored in x

    m <<- NULL
    
    # inverse is initiallized to NULL
  }

  

  # get: retrieve the current matrix

  get <- function() x
   # current matrix is retrieved
 

  setinverse <- function(inverse) m <<- inverse
  	# inverse is stored

  getinverse <- function() m
 	# current inverse is retrieved
  

  list(set = set, get = get,

       setinverse = setinverse,

       getinverse = getinverse)
       
   # special list containing the functions is returned

}


`cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix`. If the inverse has
    already been calculated,  then `Solve()` should retrieve the inverse from the cache. The inverse is cashed if not alredy present in the cache. 

cacheSolve <- function(x, ...) {

  
  m <- x$getinverse()
  
  # inverse of the matrix from the cache is retrieved

  

  if(!is.null(m)) {

    message("getting cached data")


    return(m)
    
    # return the cache matrix inverse

    # if inverse is cached the function execution stops here

    
  }

  

  data <- x$get()
  # if cache did not have our matrix inverse retrieve the matrix, or compute its inverse, or cache the inverse, or return the inverse 

  m <- solve(data, ...)

 
  x$setinverse(m)
  
  m

}
