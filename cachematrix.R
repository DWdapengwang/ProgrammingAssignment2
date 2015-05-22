##Given a matrix, a makecacheMatrix object is created that cache the matrix's inverse
##Calling cacheSolve method will get the inverse of the matrix if it exsist(from the makeCacheMatrix object),
#if not, then it will calculate the inverse


## makeCacheMatrix creat list of functions for the input matrix
## there are getter and setter method for inverse as well as the matrix it self

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        setMatrix <- function(y){    ##this function assignes x the value of input y, and set the inv to null, where x and inv value are defined outside this function
                x <<- y
                inv <<- NULL    ##if the matrix is changed, the inv is set to null
        }
        
        getMatrix <- function() x   ##gets the matrix 
        setInv <- function(inverse) inv <<- inverse    ##this function set the input value as inv value
        getInv <- function() inv    ##this function returns the inv value
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)  #returns a list with functions as the element

}


## cacheSolve gets the inverse if it exsist, if not then it will calculate the 
## inverse and store it in the makeCacheMatrix object 

cacheSolve <- function(x, ...) {
        inv <- x$getInv()    
        if(!is.null(inv)){    #if the inverse already exsist, then get the cached inverse
                message("getting the cached inverse")
                return(inv)   #inverse exsists already, returning this value and exit the function
        }
        
        ##if inv is equal to null ie does not exsist, then:
        mat <- x$getMatrix()    #get the matrix
        inv <- solve(mat, ...)   #find the inverse
        x$setInv(inv)    #get the inverse
        inv  #the returned inverse matrix
}
