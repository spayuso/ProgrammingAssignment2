## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv=NULL
    set=function(y){
        x<<-y
        inv<<-NULL
    }
    get=function(){ ##get the original matrix
        return (x)
    }
    setInv=function(inverse) ##set the inverse matrix
    {
        return (inverse)
    }
    getInv=function() ##get the inverse matrix
    {
        return (inv)
    }
    list(set=set, get=get, setInv=setInv, getInv=getInv) ##make a list with the original and inverse matrix


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv=x$getInv() ##return the inverse matrix
        if(!is.null(inv)) ##if we have inverse before, load from cache
        {
            ##use cache to return inverse
            message("Caching data to inverse matrix")
            return (inv)
        }
        
        ##else, we compute the inverse with solve function
        mat.data=x$get()
        inv=solve(mat.data,...)
        x$setInv(inv)
        return(inv) ##return inverse matrix
}
