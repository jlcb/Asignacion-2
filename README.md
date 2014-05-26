##Asignacion-2
## José Castañeda 

## Con este script se puede crear una matrix de un tipo especial con la función "makeCacheMatrix ()", esto permite recordar y obtener sus atributos. 
## Tambien calcula su inversa utilizando la función "cacheSolve L " si es necesario 

makeCacheMatrix <- function(x = matrix()) { ## Creation of matrix
	inv <- NULL   ##initiation of matrix
	set <- function(y) {  ##function values ​​in the whole matrix. if the matrix changed, its inverse is started again 
		x <<- y
		inv <<- NULL   
	}
	get <- function() x	 ##return matrix
	setinv <- function(inverse) inv <<- inverse  ## inverse matrix 
	getinv <- function() inv ##return inverse matrix.
	list(set = set,get = get, ##list of functions
		setinv = setinv,
		getinv = getinv)
}

cacheSolve <- function(x) { ##this function calcule and return the inverse matrix
	inv <- x$getinv() ##get the inverse matrix
        if(!is.null(inv)) { ##if it was calculated, return.
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ## or else get data of x 
        inv <- solve(data) ##Get Acquired data reverse
        x$setinv(inv) ##set inverse of x
        inv	##return inverse
}
============
