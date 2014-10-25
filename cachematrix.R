## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
 

## Write a short comment describing this function

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
