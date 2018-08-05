# Creates a matrix with cached data and its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL #initialization
    set<-function(y){
        x<<-y #Data is cached
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}
#---------
# used for calculating the inverse of a matrix
# For consequetives calls with a same matrix, retruns the cached inverse value instead of re-calculating,
# and prints out informative message reflecting that
# 
# Example: The inverse of the matrix
#       4   7
#       2   6
# 
# is
#       0.6  -0.7
#       -0.2  0.4
#
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    datos<-x$get()
    m<-solve(datos, ...)
    x$setmatrix(m)
    m
}
