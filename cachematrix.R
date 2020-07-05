makeCacheMatrix <- function(M = matrix()){
  N <- NULL
  
  set <- function(S){
    M <<- S
  }
  get <- function() M
  setinv <- function(solve) N <<- solve
  getinv <- function() N
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(M,...){
  N <- M$getinv()
  if (!is.null(N)){
    message("getting cached data")
    return (N)
  }
  data <- M$get()
  N <- solve(data,...)
  M$setinv(N)
  N
  ## Return a matrix that is the inverse of 'M'
}