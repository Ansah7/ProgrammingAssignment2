## Put comments here that give an overall description of what your
## functions do
## The below functions compute the inverse of a square matrix.
## It has been assumed to be invertible as only invertible matrices have inverses.
## Write a short comment describing this function
## The makeCacheMatrix is meant to set and get the matrix, set and get the inverse by storing in memory
makeCacheMatrix<- function(x = matrix()){
        invermatrix<-NULL              ##initialization of inverse
        set<-function(y){
                x<<-y                  ##setting the matrix
                invermatrix<<-NULL     ##initialize to null for the cachesolve not to use it for new matrix inverse calculations
        }
        get<-function(){               ##getting the matrix
                x
        }
        setinverse<- function(inverse){      ##seting the inverse to be stored in memory
                invermatrix<<-inverse 
        }
        getinverse<-function(){                ##calling the inverse to prevent recalculation
                invermatrix
        }
        list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
        
}


## Write a short comment describing this function
## cachesolve is to produce inverse if already calculated and compute if not
cacheSolve<- function(x,...){
        invermatrix<-x$getinverse()
        if(!is.null(invermatrix)){
                message("Getting Cached Inverse Data")
                return(invermatrix)
        }
        data<-x$get()
        invermatrix<-solve(data,...)
        x$setinverse(invermatrix)
        invermatrix
        
}
