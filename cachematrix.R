############################################################################################
##This code has two parts. The makeCacheMatrix starts with an empty n by n by 4 matrix of  #
##null values. The codes populates x with a user specified square, invertible y matrix.    #
##The makeCacheMatrix then specifies three more functions get, setinverse, and getinverse. #
##The list command populates the four levels of the x matrix with set, get, setinverse,    #
##and getinverse.                                                                          # 
#                                                                                          #
##The second part cacheSolve takes the n by n by 4 matrix, x, from part one. It first      #
##checks if the inverse has been previously calculated. If so, it returns the inverse.     #
##If not, it gets the raw data in the second layer of x (via x$get), calculates the        #
##inverse, stores the value into setinverse, and returns m.                                #
############################################################################################

makeCacheMatrix <- function(x = matrix(list(NULL),c(,,4))) {
        
	m <- NULL
      set <- function(y) {
      	x <<- y
            m <<- NULL
      }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
#####################################################################################
##Use example: Below you can test the functions above by generating an n by n matrix#
##of random numbers from a normal distribution with mean zero.                      #
#####################################################################################

n<-4
y<-matrix(rnorm(n^2),c(n,n))

##############################################################
##These lines execute the functions above usign the y matrix##
##############################################################
x<-makeCacheMatrix(y)
cacheSolve(x)
##If you run cacheSolve(x) a second time, it will return the inverse matrix stored in the cache.##
