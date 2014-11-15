## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function makeCacheMatrix takes a variable of class matrix as input and returns back a variable of class list
#The list returned has definitions of four internal functions defined here
#set --> used to store the input matrix
#get--> used to retrive the input matrix
#setinversematrix --> used to store the inverse of input matrix
#getinversematrix --> used to retrieve the inverse of input matrix
#The list also has the address of the informations stored in the environment
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #initializes the value 
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinversematrix<-function(solve) m<<- solve
  getinversematrix<-function() m
  list(set=set, get=get,
       setinversematrix=setinversematrix,
       getinversematrix=getinversematrix)
}


## Write a short comment describing this function
# CacheSolve takes the list created by makeCacheMatrix as input
# It first uses the getinversematrix call to see whether the input and output is available in cache (basically the environment)
# If available in cache retruns the inverse matrix from cache
# else first gets the input matrix using the list using get method
# Then uses the solve method to get inverse of matrix
# Then uses the setinversemeatrix to store the inverse of the matrix into the cache
# Then returns the computed inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinversematrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinversematrix(m)
  m
}
