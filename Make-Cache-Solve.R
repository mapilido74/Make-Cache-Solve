
#******************
# makeCacheMatrix *
#******************

## Create a special objet that stores a matrix and cache it's inverse

makeCacheMatrix <- function(x = matrix(), ...) {
    m_inv<- NULL # the variable in which store the inverse
    set <- function(y) { ## set the value of the matrix
        x <<- y ## assign y to x
        m_inv <<- NULL ## assign NULL to m_inv
    }
    get <- function() x ## get the value of the matrix
    setinv <- function(solve) m_inv<<-solve(x) ## set the value of the matrix inverse
    getinv <- function() m_inv ## get the value of the matrix inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) # return the "special matrix"
}

#*************
# cacheSolve *
#*************


## Calculates the inverse of the "matrix "special matrix" created with the 
## makeCacheMatrix. This function forst checks if the inverse has already been 
## calculated

cacheSolve <- function(x=matrix(), ...){
    m_inv<-x$getinv()
    if(!is.null(m_inv)) { ## Check if the inverse has already claculated
        message("getting cached data") # if the answer is yes, then shown the message
        return(m_inv) ## return the inverse in the cache
    } 
    mymatrix <-x$get() # mymatrix is the original matrix
    m_inv <- solve(mymatrix) ## calculated the inverse
    m_inv<- x$setinv(m_inv) ## set the inverse from the "special matrix"
    m_inv # return the inverse
}  

#********
# Check *
#********

C1<-1:4
C2<- c(5,9,6,4)
C3<-c(0,1,0,2)
C4<-c(3, 7, 11, 0)
C<-cbind(C1, C2, C3, C4)

x <- makeCacheMatrix()
x$set(C)
x$get()
cacheSolve(x)

I<-cacheSolve(x)

round(I%*%C, digits=3)



