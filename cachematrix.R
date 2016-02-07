## makeCacheMatrix function creates a special matrix object, which cache its inverse
## 1. set matrix value
## 2. get matrix value
## 3. set inverse matrix value
## 4. get inverse matrix value

makeCacheMatrix <- function(x = matrix()) 
{
    inver <- NULL
    set <- function(y) 
      {
        x <<- y
        inver <<- NULL
      }
    get <- function() x
    setInverse <- function(inverse) inver <<- inverse
    getInverse <- function() inver
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##------------

## cacheSolve function initially check whether inverse of the matrix already computed or not. If Computed, it gets the results and skip the computation.
## If not computed, it computes the inverse first and sets the inverse value in the cache using setInverse function.
cacheSolve <- function(x, ...) 
{
    inver <- x$getInverse()
    if(!is.null(inver)) 
      {
        print("retrieve cached data !!!")
        return(inver)
      }
    data <- x$get()
    inver <- solve(data, ...)
    x$setInverse(inver)
    inver  
}

