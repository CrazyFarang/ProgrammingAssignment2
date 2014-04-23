## Pair of functions that are used to create a special object that stores a matrix and caches its inverse. 

# makeCacheMatrix function creates special "matrix" object that can cache its inverse matrix.
# It has just one argument passed - a matrix.
# This function generates a list of 4 functions:
# 1. SET function sets the content of the matrix
# 2. GET function returns the content of the matrix
# 3. SETINVERSE function sets the content of inverse of the matrix
# 4. GETINVERSE function returns the stored inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL                       #set the inverse matrix variable to NULL
        set<-function(y){               #set the content of the matrix
                x<<-y                   
                inv<<-NULL
        }
        get<-function() x               #return the content of the matrix
        setinverse<- function(inverse) inv<<-inverse            #set the content of inverse of the matrix
        getinverse<- function() inv                             #return the stored inverse of the matrix
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)       #return list of the functions
}


## casheSolve function creates inverse of the special object "matrix" created with makeCacheMatrix function.
# The function checks if the inverse of the matrix was already created, in that case it gets the inverse of the matrix from the cache and doesn't do any computations.
# If the inverse of the matrix is not available, it computes the inverse of the matrix and sets it content in the cache with SETINVERSE function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()                             #query the matrix cashe
        if (!is.null(inv)) {                            #if there is cashe available
                message("getting cashed data")
                return(inv)                             #return the cashed data
        }
        m<-x$get()                                      #if there is no cashe
        inv<- solve(m)                                  #compute the inverse matrix
        x$setinverse(inv)                               #save the result - inverse matrix back to the matrix cashe
        inv                                             #return the result
}
