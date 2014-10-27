## Method that will help us calculate inverse of a matrix fast by caching the result.
# 'set' and 'get' are getter and setter for matrix
# 'getinverse', and 'setinverse'are getter and setter for getting inverse of set matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  #Setter method for inverse
  setinverse <- function(inverse) m <<- inverse
  #Getter method for inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
 

## Write a short comment describing this function
# Function return inverse value if already calculated.
# Also, Function return null if inverse is not possible by calculating the determinent.
# If determinent is 0, then inverse is not possible. So, inverse value return will be NULL

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- if( det(data) != 0 ){
           solve(data, ...)
        }else{
          print("Inverse is not possible of matrix with determinat 0")
          NULL
        } 
  x$setinverse(m)
  m
}

################################ Example Run
# 
# > c=rbind(c(1, -1/4), c(-1/4, 1))
# > c
#       [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# 
# > mat <- makeCacheMatrix(c)
# > ls(mat)
# [1] "get"        "getinverse" "set"        "setinverse"
# > mat$getinverse()
# NULL
# 
# > cacheSolve(mat)
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > mat$get()
#       [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# > mat$getinverse()
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
################################
