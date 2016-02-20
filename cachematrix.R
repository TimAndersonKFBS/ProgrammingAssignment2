## These functions satisfy Assignment 2: Lexical Scoping

## The first function creates a special matrix object that is able to cache its inverse
## Inside this function we define four smaller functions, each one performs a subset of the cache process: Set, Get, Setmatrix, and GetMatrix
      
      makeCacheMatrix <- function(x = matrix()) {   
            mat <- NULL                             
            set<-function(y){                       ##Our first fucntion passes a value passed to it out of this function to x in the global environment
                  x <<- y
                  mat <<-NULL                       
            }
            get<-function() x                         ## Very simply returns the x variable we put out to the global environment
            setmatrix<-function(solve) mat <<- solve  ## Here we pass the solution given to us from the function below to the variable mat
            getmatrix<-function() mat                 ## Just pulling the varible mat from beyond this function
            
            ## This line below makes it easier to call the four functions above
            list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
            
      }
      
      
## This function will report back the inverse of a matrix
## first, it looks to see if it has been cached, if not it will calculate it and set the cache
## It's designed to be passed the matrix created by the function above.  It assumes that the matrix supplied is always invertible
      
      cacheSolve <- function(x, ...) {
              
      ## First we use the getmatrix portion of the function above
        mat <- x$getmatrix()
        
        if(!is.null(mat)){          ## If we get back a real matrix we report that we have success and report the matrix
          message("getting cached data")
          return(mat)
        }
        
        ## The only way we get here is if the mat variable was null...so we get the original matrix x, use the solve function to 
        ## Calculate the inverse matrix, use the setmatrix function to put our calculation in the cache, and finally report the mat variable
        
        matrix <- x$get()
        mat <- solve(matrix, ...)
        x$setmatrix(mat)
        mat
        
      }
      
      
      
      ## sample run
      ## > m <- matrix(1:4,2,2)
      ##> m
      ## [,1] [,2]
      ## [1,]    1    3
      ## [2,]    2    4
      ## > 
      ## > z <- makeCacheMatrix(m)
      ## > z$get()
      ## [,1] [,2]
      ## [1,]    1    3
      ## [2,]    2    4
      ## > 
      ## > z$getmatrix()
      ## NULL
      ## > 
      ## > cacheSolve(z)
      ## [,1] [,2]
      ## [1,]   -2  1.5
      ## [2,]    1 -0.5
      ## >
      ## > z$getmatrix()
      ## [,1] [,2]
      ## [1,]   -2  1.5
      ## [2,]    1 -0.5
      ## > cacheSolve(z)
      ## getting cached data
      ## [,1] [,2]
      ## [1,]   -2  1.5
      ## [2,]    1 -0.5
      ## > 
