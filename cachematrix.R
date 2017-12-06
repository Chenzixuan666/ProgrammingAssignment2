## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 创建一个特殊的列表
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## set 用来重新设置x值
    ## <<-号是全局赋值，将y赋给输入的x
    ## 而不是创建一个局部的x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ##返回x
    ##function后面不加大括号也可以
    get <- function() x
    ##将inverse值赋给i
    setinv <- function(inverse) i <<- inverse
    ##返回i
    getinv <- function() i
    #返回一个列表
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## 与上面的函数配合，x只能是make出来的那种
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    ##判断是否已经计算过，若计算过，直接输出
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ##若没有计算过，则计算并储存
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
test1 <- matrix(c(1,2,3,4),2,2)
my_Matrix <- makeCacheMatrix(test1)
cacheSolve(my_Matrix)

test2 <- matrix(c(1, 8, 7, 8),2,2)
my_Matrix2 <- makeCacheMatrix(test2)
cacheSolve(my_Matrix2)