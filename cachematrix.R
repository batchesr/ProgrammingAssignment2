##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        i<-NULL
        
set<-function(y=matrix()) {             ## Set function caches the matrix y to x
        x<<-y
        i<<-NULL
}
get<-function() {                       ## Get function returns back the matrix x that has been cached
        return(x)        
}
setInv<-function(inv) {                 ## SetInv function caches the inverse matrix of x
        i<<-inv
}
getInv<-function() {                    ## GetInv function returns back the cached inverse
        return(i)
}
list(set=set,get=get,setInv=setInv,getInv=getInv)

}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...)
{
        i<-x$getInv()                   ## Calls back the cached inverse matrix from above
        if(!is.null(i))                 ## If the called back matrix exists, it will print "getting cached matrix"
        {                               ## and return the inversed matrix
                message("getting cached data")
                return(i)
        }
        data<-x$get()                   ## If not, calls back the original matrix and solves for the inverse
        i<-solve(data,...)              ## using the solve function (for square matrices)
        x$setInv(i)
        i                               ## Returns the solved inverse matrix
}
