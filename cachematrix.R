## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix cretes a list of functions: set(y), get, setinv(m_inv) e getinv; set: set the matrix to y; get: gives back the matrix x; setinv: set the
## inverse of x to 
## m_inv and getinv returns the inverse of x
## cacheMatrix: evaluates the inverse of the matrix and cached the result 

## Write a short comment describing this function

## This function create a new object, which can be seen as a 
## special matrix that can cache its inverse; it can take only one argument, which is a matrix.
## The fact that we are calling the argument x in the function definition creates the variable x in the object. 
## If we include an argument when we call the function, i.e.: special_matrix <- makeCacheMatrix(matrix(c(1:9),3,3)),
## the object special_matrix would contain a matrix x with 3 rows and 3 columns with numbers from 1 to 9;
## if we don't pass an argument then x is created as an empty matrix; then we can set the values of x through the function set


makeCacheMatrix <- function(x = matrix()) {
## inv is the matrix where the inverse of x is chached;  
	inv <- NULL 

## this function set the matrix x to y and its inverse to NULL; 
## thanks to the  operator <<- we can refer to variable that already exist in some "parent" context: this means that inv and x will have these new value also in the 
## R session 
	set <- function(y){
		x <<- y
		inv <<- NULL
	}

## this function return the matrix x
	get <- function() x

## this function set the inverse of x (inv); notice that we are using the operator <<- to change the value of inv also in the parent directory 
	setinv <- function(m_inv) inv <<- m_inv

## this function return the inverse 	
	getinv <- function() inv

## then, this command creates a list containg all the functions; this is want is returned from the function makeCacheMatrix
	list(set=set,get=get,setinv=setinv,getinv=getinv) 
} 

## Write a short comment describing this function


## This function returns the inverse of the matrix x if it has already been evaluated, othervise it evaluates the inverse and chached into inv; the dots are 
## the arguments of solve; we are supposing that the matrix x is invertible

cacheSolve <- function(x, ...) {
## get the inverse         
	x_inv <- x$getinv()
## if it is not Null it does not evaluate the result but takes from cached data
	if(!is.null(x_inv)){
	print("getting inverse from cached data")
	return(x_inv)
	}
## if it arrives here it means that the inverse has never been evaluated, so it evaluates and caches the results
	data <- x$get()
      x_inv <- solve(data, ...)
      x$setinv(x_inv)
      x_inv
}
