
## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.Thus we have created two functions, "makeCacheMatrix" and "cacheSolve".


### makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(nowm = matrix()) {
        cachedinverse <- NULL
        
        
## Sub-function no. 1
## Promote a new matrix "newm" to "nowm" in the whole "makeCacheMatrix" function environment (<<- operator), so that sub-function no.2 inside this environment will now return the value of "newm".
# Set value of "cachedinverse" in the whole environment (<<-operator) to NULL to delete the inverse matrix of the former(now replaced) "nowm" matrix.
        
        setmatrix <- function(newm) {
                nowm <<- newm                     
                cachedinverse <<- NULL
        }

## Sub-function no. 2

        getmatrix <- function() nowm
        
        
## Sub-function no. 3
## Save/cache the latest value of the inverse matrix calculated by the "cacheSolve" function below into the "cachedinverse" object in the whole "makeCacheMatrix" function environment (<<- operator), so that subfunction no.4 inside this environment will now return the latest value of the inverse matrix. 
       
        setinverse <- function(latestinverse) cachedinverse <<-latestinverse
        
## Sub-function no. 4
        
        getinverse <- function() cachedinverse
        
        
        list(setm = setmatrix,
             getm = getmatrix,
             setin = setinverse,
             getin = getinverse)
}
                        


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(CacheMatrix, ...) {

## Say "makeCacheMatrix" was passed as "CacheMatrix"
## Retrieve the latest calculated inverse matrix saved in the object "cachedinverse" in "makeCacheMatrix" function environment into the "cacheSolve" function environment.        
        cachedinverse <- CacheMatrix$getin()
        
## Value of "cachedinverse" is NULL if "makeCacheVector" or "makeCacheVector$setm" is called. If so, return the existing value of "cachedinverse" retrieved from the "makeCacheMatrix" function environment instead of recalculating.
        
        if(!is.null(cachedinverse)) {
                message("getting cached data")
                return(cachedinverse)
        }
        
## Otherwise, recalculate a new inverse from the current latest matrix ("nowm") from the "makeCacheMatrix" function environment. 
        
        data <- CacheMatrix$getm()
        newcalculatedinverse <- solve(a=data, ...)
        
## Export/cache the latest calculated value of the inverse matrix into the "cachedinverse" object in the "makeCacheMatrix" function environment (<<- operator). Any existing value of "cachedinverse" will be overwritten. Subfunction no.4 inside the "makeCacheMatrix" function environment will now return the latest calculated value of the inverse matrix.     
        
        CacheMatrix$setin(newcalculatedinverse)
        print(newcalculatedinverse)
}
