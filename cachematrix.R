##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        i<-NULL
        
set<-function(y=matrix()) {
        x<<-y
        i<<-NULL
}
get<-function() {
        return(x)        
}
setInv<-function(inv) {
        i<<-inv
}
getInv<-function() {
        return(i)
}
list(set=set,get=get,setInv=setInv,getInv=getInv)

}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...)
{
        i<-x$getInv()
        if(!is.null(i))
        {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setInv(i)
        i
}
