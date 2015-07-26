## This R-script describe how we can store an object in a different environment and call them when they are needed. 
# 
## First Function shows how an matrix object can be constructed to store its inverse together with its original Matrix.
#  The second function cacheSolve shows how matrix object constructed in the makeCacheMatrix function can be 
#  initialized with an inverse value. 

# Matrix Object Constructor
makeCacheMatrix <- function(x = matrix()) 
{
  Inv <- NULL
  
  get <- function() x # return the original data
  
  # set function allows user to change the original matrix anytime
  # every change made to original will reset the Inv value to NULL.
  # The reset is necessary because we do not want cacheSolve to return inversed Matrix of the old data. 
  set <- function(y) {
    x <<-y 
    Inv <<- NULL
  }
  
  # Here assign the Inv with the Inversed matrix value. The inversed matrix value may be from a different environment
  setInverse <- function(Inverse_Value) Inv <<- Inverse_Value 
  
  # return the Inv
  getInverse <- function() Inv
  
  # return the list of function 
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
  
}


## Matrix Object Solver/Initializer
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  real_Inverse <- x$getInverse()
  # First part check to see if the Inversed Matrix is already calculated
  if (!is.null(real_Inverse)) 
  {
    print("Inverse is already set")
    return(real_Inverse)
  }
  # Second part, if Inversed Matrix is not calculated, get the original data, and solve the matrix. 
  else
  {
    original_data <- x$get()
    
    real_Inverse <- solve(original_data) # note: Here we should use tryCatch. However, since
    # we are assuming all input are invertible then I guess its not necessary. The function will be more prone
    # to errors though. 
    x$setInverse(real_Inverse)
  }
  real_Inverse
}

#-------------------Result Demostration----------------------#
#   > source('~/Documents/PA2/ProgrammingAssignment2/cachematrix.R')
#   > trial1 <- makeCacheMatrix(a)
#   > cacheSolve(trial1)
#           [,1] [,2]
#     [1,]   -2  1.5
#     [2,]    1 -0.5
# 
#  Here is the Inverse set by the function cacheSolve
#   > trial1$getInverse()
#           [,1] [,2]
#     [1,]   -2  1.5
#     [2,]    1 -0.5
# 
#   Here is the original Square Matrix
#   > trial1$get()
#           [,1] [,2]
#     [1,]    1    3
#     [2,]    2    4
#   
#  The follow trials shows how the matrix object can be set with a new value
#  and how cacheSolve can return the inversed matrix of the new value.
#   > a
#           [,1] [,2]
#     [1,]    1    3
#     [2,]    2    4
#   > b
#           [,1] [,2] [,3]
#     [1,]    1    6    4
#     [2,]    2    1    5
#     [3,]    4    3    9
# 
#  > trail2$get()
#           [,1] [,2]
#     [1,]    1    3
#     [2,]    2    4
# 
#  >trail2$set(b)
#  >trail2$getInverse()
#    NULL
# 
#  >cacheSolve(trail2)
#             [,1] [,2]       [,3]
# [1,] -0.4285714 -3.0  1.8571429
# [2,]  0.1428571 -0.5  0.2142857
# [3,]  0.1428571  1.5 -0.7857143



