makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
#########output:
>source("demo.R")
> my_matrix <- makeCacheMatrix(matrix(1:4,2,2))
> my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> my_matrix$getsolve()
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(my_matrix)
getting inversed matrix
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$getsolve()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
