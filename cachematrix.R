

#---------------------------------------FUNCTION: makeCacheMatrix---------------------------------------#
# This function creates a special matrix, a list, containing the following functions
# Set the values of the matrix
# Get the values of the matrix
# Set the values for Inverse of the matrix
# Get the values for Inverse of the matrix 
#-------------------------------------------------------------------------------------------------------#

makeCacheMatrix <- function(myMatrix){
  
        myMatrixInverse <- matrix(NA, nrow(myMatrix),ncol(myMatrix))          #Initialize the inverse matrix with "NAs"
    
        set <- function(myOtherMatrix){                                
                myMatrix <<- myOtherMatrix                                    #Override special matrix by the matrix passed via "set" 
                myMatrixInverse <<- matrix(NA, nrow(myMatrix),ncol(myMatrix)) #re-assign (Override) NAs to inverse matrix
        }
        
        get <- function() myMatrix                                            #Return special matrix 
        setInverse <- function(setInverse) myMatrixInverse <<- setInverse     #Override the inverse matrix by the matrix passed via "setInverse"
        getInverse <- function() myMatrixInverse                              #Return the inverse matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#-----------------------------------------FUNCTION: cacheSolve------------------------------------------#
#This function creates an inverse of the special matrix created in function makeCacheMatrix
#If an inverse was already calculated, it retreives the inverse from the cache
#-------------------------------------------------------------------------------------------------------#

cacheSolve <- function (myMatrix){

        myMatrixInverse <- myMatrix$getInverse()     #Retrieve inverse via getInverse
        #print(myMatrixInverse[1,1])
  
        if (!is.na(myMatrixInverse[1,1])){           #Check if the first node of the inverse is NA (only possible if inverse was not calculated earlier)
                message("Getting cached data")       #Retrieve and return cache data if inverse was computed earlier
                return(myMatrixInverse)
        }
  
        myData <- myMatrix$get()                     #Otherwise Compute inverse
        myMatrixInverse <- solve(myData)
  
        myMatrix$setInverse(myMatrixInverse)         #Set the computed inverse via setInverse
        myMatrixInverse                              #Return the inverse
}

#----------------------------------------------OUTPUT TEST-----------------------------------------------#
# a <- matrix(1:4,2,2)                  Create a square matrix called "a"
# b <- makeCacheMatrix (a)              Create a special matrix "b" by calling makeCacheMatrix on "a"
# cacheSolve (b)                        Call cacheSolve on "b"
# cacheSolve (b)                        It would surely get you the inverse from the cache!
#--------------------------------------------------------------------------------------------------------#
