my function must make cache the reversed matrix

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



## Write a short comment describing this function

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

