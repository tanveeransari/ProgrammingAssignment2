## makeCacheMatrix allows getting setting the matrix and its inverse
## It caches the value in variable inv

## cacheSolve checks and returns cached value if it exists, else matrix inverse

## inv variable is the cached inverse
## makeCacheMatrix is a list containing functions to
## get & set value of matrix, get & set value of inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y) {
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  
  setinv <- function(i) inv<<- i
  
  getinv <- function() inv
  
  list(get=get,set=set, getinv=getinv, setinv=setinv)
}


## Check if inv already calculated using getinv()
## If so return cached value, otherwise
## solve for matrix inverse, set it and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
