## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL

       set <-function(y){
               x <<- y
               i <<- NULL
       } 
        get<-function() x
        
        setinverse <-function(m_inv) i<<- m_inv
        getinverse <- function() i
        
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        z<-x$getinverse()
        
        if(!is.null(z))  {
                
                message("getting cached data")
                return(z)
        }
        else{
                
               B<- x$get() 
               i<-solve(B)
               x$setinverse(i)
               i
        }
}
