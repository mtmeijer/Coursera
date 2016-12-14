## Programming assignment 2 - Coursera course "R programming",  week 3
## By MTM

## The first function, makeCacheMatrix, defines a new, special "matrix", which is really a list containing the functions to

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the inverse of the matrix
#4. get the inverse of the matrix

#Together with the cacheSolve function, this will allow for the storage of inverse matrices in cache, so that they do not need to be 
#re-calculated every time they are required for down-stream applications

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
#defines x as a matrix/function element. Creates empty variable inv for later use
        
        set <- function(y) {                
                x <<- y                     
                minv <<- NULL   
                
                #defines x as the input ik makeCacheMatrix(), and sets minv (which is to obtain the matrix inverse) to NULL, 
                #so that if minv was calculated for a previous matrix, this wrong value will not be returned & a new inverse matrix 
                #will be created. By using <<-, these values are defined in the parent environment so that they will remain available after
                #set() closes
                #inclusion of this function also allows the vector to be changed after the cacheSolve function has been initiated, without 
                #calling the entire function. i.e. when a <- makeCacheMatrix(matrix1), using a$set <- matrix2 will change the input matrix within
                #the makeCacheMatrix object
        }
                
             
        get <- function() x
                #defines the first 'getter' part of the function. because "x" is not defined in "get", it is obtained from
                #the parent environment i.e. defined as the matrix used as input in the function
        
        setminv <- function(solve) minv <<- solve 
                #assigns 'minv' the value of 'solve', in the parent directory so that again, it remains available when setminv is completed
        
        getminv <- function() minv
                #like 'get', this line simply obtains the minv defined previously
        
        
        list(set = set, get = get,          
             setminv = setminv,
             getminv = getminv)
        
        #Here, the functions are defined as an S3 object. This object contains a list with all the functions defined above, but also the
        #"state" in which it was created i.e. it knows the variables defined when the function was called (i.e. x and minv)
        #(per definition, something is an 'object' when it contains both 'state' (= what it 'knows') and 'behaviour' (what it 'does))
        #the "a = a" arguments name each argument in the list, so that they can be retrieved with the $ operator, not just the [[]] indexing
       
}
        
        
        
                
                


## This function will check if the inverse matrix requested is cached, and if so will return the cached data. 
## The inverse matrix will not be re-calculated.
## If there is no cached data,the inverse matrix will be calculated and displayed.

## For important notes: see comments below the code

cacheSolve <- function(x, ...) {        #Opens the function, which needs the output from makeCacheMatrix as input (so NOT a matrix!)
        
        minv <- x$getminv()             #sets minv within this function to the same value as the resultant value in the makeCacheMatrix function
                                        # if there is no inverse matrix previously calculated, this will return the NULL value to which minv is
                                        # set at the start of the function
        
        if(!is.null(minv)) {                                 
                message("getting cached data")  
                return(minv)
        }
                                        #if: minv is not NULL (i.e. it was assigned a value above, i.e. the inverse matrix has previously been calculated)
                                        #this statement will return the message 'getting cached data', and will then display the cached inverse matrix
                                        #and skips the computation below.
        
        data <- x$get()                 #assigns the original input matrix to 'data'
        minv <- solve(data, ...)        #creates the inverse matrix using the solve() function, and stores it as minv
        x$setminv(minv)                 #sets minv (within the object makeCacheMatrix) to minv (calculated in the line above)
        minv                            #prints minv / the inverse matrix calculated
        
}

## Important notes: 

# cacheSolve does not take a matrix as input, but an S3 object created by makeCacheMatrix
# When makeCacheMatrix is used within the cacheSolve function - i.e cacheSolve(makeCacheMatrix(matrix1)) - there will never be cached data
# as each repetition of this command will create a new S3 object, which will every time start with setting 'minv' to NULL.
# to obtain cached versions use 
# avariable <- makeCacheMatrix(matrix1)
# cacheSolve(avariable)

# when the latter line is re-ran, it will obtain the cached data. If a new matrix is used - e.g. aVar <- makeCacheMatrix(matrix2) - 
# it will re-calculate the inverse matrix (even when matrix1 == matrix2).)
# Moreover, if matrix1 is later edited within the S3 object - using aVar$set(matrix3), the inverse matrix will be re-calculated on the 
# first call of cacheSolve(aVar) (even when matrix 3 == matrix 1)

