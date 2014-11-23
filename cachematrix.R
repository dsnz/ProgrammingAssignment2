## Put comments here that give an overall description of what your
## functions do

# set of functions to
# create object able to store a matrix and its cached inverse
# and get the inverse of this matrix (which is cached in first call)

## Write a short comment describing this function

# create object containing matrix, its inverse and a few helper functions
# initially inverse is NULL
# the helper functions
# 'get/set' return/set the matrix
# 'getinv/setinv' return/set the inverse

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(new_mat) {
    mat <<- new_mat
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(a_inv) inv <<- a_inv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

# return the inverse of the matrix object (and cache it for future requests)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}

# unittests

#source("cachematrix.R")

print("amatrix = makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow=2, ncol=2))")
amatrix = makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow=2, ncol=2))

print("print(amatrix$get())")
print(amatrix$get())         # Returns original matrix
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

print("print(cacheSolve(amatrix))")
print(cacheSolve(amatrix))   # Computes, caches, and returns    matrix inverse
#    [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

print("print(amatrix$getinv())")
print(amatrix$getinv())  # Returns matrix inverse
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

print("print(cacheSolve(amatrix))")
print(cacheSolve(amatrix))   # Returns cached matrix inverse using previously computed matrix inverse
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

print("amatrix$set(matrix(c(0, 5, 99, 66), nrow=2, ncol=2))")
amatrix$set(matrix(c(0, 5, 99, 66), nrow=2, ncol=2)) # Modify existing matrix

print("print(cacheSolve(amatrix))")
print(cacheSolve(amatrix))   # Computes, caches, and returns new matrix inverse
#            [,1] [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0

print("print(amatrix$get())")
print(amatrix$get())         # Returns matrix
#     [,1] [,2]
#[1,]    0   99
#[2,]    5   66

print("print(amatrix$getinv())")
print(amatrix$getinv())  # Returns matrix inverse
#            [,1] [,2]
#[1,] -0.13333333  0.2
#[2,]  0.01010101  0.0

print("print(cacheSolve(amatrix))")
print(cacheSolve(amatrix))   # Returns cached matrix inverse using previously computed