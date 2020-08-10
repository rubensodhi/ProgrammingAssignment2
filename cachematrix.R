## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCachematrix function is to make an object inv to cache the 
##inverse of a matrix


makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL 
##set is used to set the cache value y to x and cache value null to 
##inv for a new matrix  
  set<- function(y)
  {
    x<<- y
    inv<<- NULL
  }
##get is used to return or get the matrix x so we can use it in cacheSolve 
  get<- function() 
  {
    x
  }
##setInv function is used to set a cache value to inv that is to be
##passed on by cacheSolve function  
  setinv<- function(invmat)
  {
    inv<<- invmat
  }
##getinv function is used to return or get the inverse of matrix x
  getinv<- function(inv) 
  {
    inv
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
##this is to refer to functions using $ operator
}


## Write a short comment describing this function
##cacheSolve function is to compute the inverse matrix returned by 
##makeCacheMatrix or retrieve the inverse of a matrix passed through x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv() 
##here x$getinv gets the inverse of the matrix stored
##if statement to check whether inv to check if inv is null or not, 
##if inv is not null it shows a message and returns inv
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  matrix<-x$get()
##here matrix object copies x matrix through get function
  
  inv<- solve(matrix,...)
##here inv stores the inverse of the matrix x using solve function
  
  x$setinv(inv)
##after inverse is solved inv is passed to setinv function 
  
  inv 
##at the end inverse of the matrix is returned
}

