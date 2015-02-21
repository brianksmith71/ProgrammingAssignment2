# Quick Unit Test adapted from Jules Stuifbergen on Coursera R Programming Discussion thread:  https://class.coursera.org/rprog-011/forum/thread?thread_id=405#post-1719

source("cachematrix.R")
#
# generate matrix, and the inverse of the matrix.
size <- 1000 # size of the matrix edge, don't make this too big
mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
mymatrix.inverse <- solve(mymatrix)
#
# now solve the matrix via the cache-method
#
special.matrix <- makeCacheMatrix(mymatrix)
#
# this should take long, since it's the first go
print(system.time(special.solved.1 <- cacheSolve(special.matrix)))
#
# this should be lightning fast
print(system.time(special.solved.2 <- cacheSolve(special.matrix)))
#
# check if all solved matrices are identical
identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2)
#
# should return TRUE

#clean up the environment
rm(list=ls())

