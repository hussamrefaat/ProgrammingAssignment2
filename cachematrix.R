## Put comments here that give an overall description of what your
## functions do

## this function create a matrix and set inverse in parent enviroments , get inverse , set and get the value

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


## this function is to calculate a matrix inverse using function Solve ,if inverse value is already set gets it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        z<-x$getinverse()
    ##check if there is value already cached for matrix inverse     
        if(!is.null(z))  {
                
                message("getting cached data")
                return(z)
        }
        else{
       ##no cached value so it calculates the inverse save it in variable i and then save it using setinverse defined in the solve cache matrix function         
               B<- x$get() 
               i<-solve(B)
               x$setinverse(i)
               i
        }
}
