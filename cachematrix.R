#######################################################################################################################
##                                                                                                                   ##
## Matrix inversion can be computationally intensive, so best to reuse previous calculation if available             ##
##                                                                                                                   ##
## This function accepts a function as an argument, and calculates the inverse if a cached version is not available  ##
##                                                                                                                   ##
#######################################################################################################################

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize objects and pass to parent environment for later accessing
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## define functions to manipulate matrix- use R's logical scoping to store/retrieve from parent environment
  get <- function() x
  setsolved <- function(solve) m <<- solve
  getsolved <- function() m
  ## Name the elements in the list, else we cannot use the $ operator to extract data
  list(set = set, get = get, setsolved = setsolved, getsolved = getsolved)
}


## We pull the cached solution if available, else calculate the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m = x$getsolved()
  ## If solution exists, return solution
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Else, get input matrix and store in data object
  data <- x$get()
  ## Calculate inverse and store in m
  m <- solve(data, ...)
  ## Set the inverse in the input object
  x$setsolved(m)
  ## Return inverse to parent environment
  m
}



##   Generate random matrix on ask to test caching function  
##!!!!!!!Does not work if 2 selected before run once!!!!!!!!!!
testMatrix <- function() {
  print("Guaranteed New 6x6 Matrix?:    1 = Yes; 2 = No")
  choice <- readline()
  startTime = Sys.time();
  if (choice == 1 || run_once != 1) {
    startMatrix <<- matrix(rnorm(36), nrow = 6, ncol = 6)
    run_once <<- 1;
    temp <<- makeCacheMatrix(startMatrix)
  }
  print("Original: ")
  print(temp$get())
  print('Inverted: ')
  print(cacheSolve(temp))
  endTime = Sys.time()
  print(c('Calculation Time', endTime - startTime))
}