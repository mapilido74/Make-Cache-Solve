#******************
# makeCacheMatrix *
#******************

makeCacheMatrix <- function(x = matrix(), ...) {
    m_inv<- NULL
    set <- function(y) {
        x <<- y ## asigno el valor de y a x
        m_inv <<- NULL ## asigno NULL a m_inv
    }
    get <- function() x #funcion para conseguir x
    setinv <- function(solve) m_inv<<-solve(x)
    getinv <- function() m_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#*************
# cacheSolve *
#*************

cacheSolve <- function(x=matrix(), ...){
    m_inv<-x$getinv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    } 
    mymatrix <-x$get() # mymatrix es igual a la matriz original 
    m_inv <- solve(mymatrix) # calculo la inversa
    m_inv<- x$setinv(m_inv) # retorno la inversa
    m_inv
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



