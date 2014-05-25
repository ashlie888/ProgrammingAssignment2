## Caching the Inverse of a Vector
## 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){  #set the value of the vector
                x <<- y
                m <<- NULL
        }
        get <- function() x  #get the value of the vector  
        setmean <- function(mean) m <<- mean  #set the value of the mean
        getmean <- function() m               #get the value of the mean
        list(set = set, get = get, 
             setmean = setmean,
             getmean = getmean)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                m
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

a <- makeVector(1:7) # create an object called a 
a # shows that a is now a list of functions
class(a) # shows that a is a list 
class(a$set) # show that the elements of the list are functions
a$set(c(1,2,3,4,5,6,7)) # set the vector
a$get() #get the vector
cachemean(a)  # calculate the mean
cachemean(a)  # when is called back use the cached mean


amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getinverse()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinverse()  # Returns matrix inverse
