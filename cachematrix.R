# This is the second Programming Assignment for the Coursera R Programming Course
# This bit of code contains two functions - one to cache the inverse of a matrix and the other to search for a cached value or, if it cannot find it, produce the inverse of the matrix

# This function calculates and caches the inverse of a choosen matrix
makeCacheMatrix <- function(x = numeric()) {
      
      # First, provide a local default value for CacheMatrix
      CacheMatrix <- NULL     
      
      # Sets x and resets CacheMatrix globally 
      set <- function(y) {    
            x <<- y
            CacheMatrix <<- NULL
      }
      
      # sets three functions within the makeCacheMatrix function
      get <- function() x
      setmatrix <- function(solve) CacheMatrix <<- solve
      getmatrix <- function() CacheMatrix
      
      # Sets a list for the functions will be calling in cacheSolve
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

# This function searchs for a cached value and, if it cannot find it, produce the inverse of the matrix
cacheSolve <- function(x, ...) {
      
      # Attempts to retrieve CacheMatrix - which will receive a value if makeCacheMatrix has been performed
      CacheMatrix <- x$getmatrix()
      
      # if CacheMatrix is not empty then will retrieve cached data
      if(!is.null(CacheMatrix)) {
            message("getting cached data")
            return(CacheMatrix)
      }
      
      # will grab the data using the get function, inverse the data and place in CacheMatrix, and will output the cached or produced value
      data <- x$get()
      CacheMatrix <- solve(data)
      x$setmatrix(CacheMatrix)
      CacheMatrix
}


# everything below is for testing

z = matrix(c(2,2,3,2),nrow=2,ncol=2)
makeCacheMatrix(z)

debug(cacheSolve)
cacheSolve(makeCacheMatrix(z))

debug(makeCacheMatrix)
makeCacheMatrix(z)
