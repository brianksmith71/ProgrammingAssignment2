

#########################################################################################################
# Solution for Coursera R Programming                                                                   #
#     Programming Assignment 2 with Peer Evaluation                                                     #
#                                                                                                       #
#     Written in RStudio, Version 0.98.109 and R x64 3.1.2                                              #
#     System is Windows 7 x64 Ultimate SP 1, Intel Core i7-4700MQ CPU @ 2.40 GHz and 32.0 GB of RAM     #
#                                                                                                       #
#     Author:  Brian K Smith                                                                            #
#     Date:  2014-02-22                                                                                 #
#########################################################################################################


## Write a short comment describing this function

############################################################
#                                                          #
# Function:  makeCacheMatrix                               #
#                                                          #
# Comments:  This function works in conjunction with       #
#            another function "cacheSolve" to create an    #
#            essentially global matrix and its inverse.    #
#                                                          #
#            The point of this is conceptual, certainly    #
#            not practical as this functionality simply    #
#            shows lexical scoping in R.                   #
############################################################

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL  #set m to nothing locally
    
    #set function
    set <- function(y) 
    {
        x <<- y  #use super-assignment to increase scope
        m <<- NULL  #use super-assignment to increase scope beyond local
    }
    
    #get function
    get <- function()
    {
        return(x)
    }
    
    #setSolve
    setSolve <- function(solve) 
    {
        m <<- solve  #branches to cacheSolve
    }
    
    #getSolve
    getSolve <- function() 
    {
        return(m)  #returns super-assigned m
    }
    
    #default - return list of functions available
    list(set = set, 
         get = get, 
         setSolve = setSolve, 
         getSolve = getSolve)
}


############################################################
#                                                          #
# Function:  cacheSolve                                    #
#                                                          #
# Comments:  This function works in conjunction with       #
#            another function "makeCacheMatrix" to create  #
#            an essentially global matrix and its inverse. #
#                                                          #
#            The point of this is conceptual, certainly    #
#            not practical as this functionality simply    #
#            shows lexical scoping in R.                   #
############################################################

cacheSolve <- function(x, ...) 
{
    # return super-assigned m
    m <- x$getSolve()
    
    #if matrix exists, print message and return m
    if (!is.null(m)) {
        message("getting cached data")
        
        return(m)
    }
    
    # get original matrix
    data <- x$get()
    
    # call to get inverse of matrix
    m <- solve(data, ...)
    
    # set inverse matrix
    x$setSolve(m)
    
    # return inverse matrix
    return(m)
}


## Example run:
##
## > source ("cachematrix.R")
## > my_matrix = matrix( c(1, 1, 3, 1), nrow=2, ncol=2)
## > x <- makeCacheMatrix(my_matrix)
## > x$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    1    1
## > cacheSolve(x)
## [,1] [,2]
## [1,] -0.5  1.5
## [2,]  0.5 -0.5
## > cacheSolve(x)
## getting inversed matrix
## [,1] [,2]
## [1,] -0.5  1.5
## [2,]  0.5 -0.5
## >
## > my_matrix = matrix( c(1, 1, 2, 4, 1, 3, 2, 2, 1), nrow=3, ncol=3)
## > x$set(my_matrix)
## > x$get()
## [,1] [,2] [,3]
## [1,]    1    4    2
## [2,]    1    1    2
## [3,]    2    3    1
## > cacheSolve(x)
## [,1]       [,2]       [,3]
## [1,] -0.5555556  0.2222222  0.6666667
## [2,]  0.3333333 -0.3333333  0.0000000
## [3,]  0.1111111  0.5555556 -0.3333333
## > cacheSolve(x)
## getting inversed matrix
## [,1]       [,2]       [,3]
## [1,] -0.5555556  0.2222222  0.6666667
## [2,]  0.3333333 -0.3333333  0.0000000
## [3,]  0.1111111  0.5555556 -0.3333333
## > 


## Write a short comment describing this function

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## Write a short comment describing this function

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}