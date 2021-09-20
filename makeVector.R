# Fiona
makeVector <- function(x = numeric()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
   }
   get <- function() x
   setmean <- function(mean) m <<- mean
   getmean <- function(mean) m
   list(set = set, get = get,
      setmean = setmean,
      getmean = getmean)
}

VectorSolve <- function(x,...) {
  f <- x$getmean()
  if(!is.null(f)){
    message("getting cached data")
    return(f)
  }
  data <-x$get()
  f <- solve(data,...)
  x$setmean(f)
  f
}
