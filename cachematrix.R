## function makes a special matrix

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setreverse <- function(solve) m <<- solve
                getreverse <- function() m
                list(set = set, get = get,
                     setreverse = setreverse,
                     getreverse = getreverse)
        }



## The following function calculates the reverse of the matrix created with the above function.

cacheSolve <- function(x, ...) {
                m <- x$getreverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setreverse(m)
                m
        }
               ## Return a matrix that is the inverse of 'x'

