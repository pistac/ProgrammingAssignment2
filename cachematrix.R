## The makeCacheMatrix function creates an environment with a given matrix, whose inverse the function cacheSolve will return. The advantage of cacheSolve is that 
## it won't calculate the inverse each time it is called, but only the first time after the matrix is created or changed, thus saving processing time and resources.

# The function makeCacheMatrix creates an environment with 4 different functions (set, get, setinversemx, getinversemx) that can be accessed from the outside;
# it also contains two variables (x and inversemx) that cannot be accessed from the outside. 
# If the matrix is modified at a later time, this function also sets the inverse to null.

makeCacheMatrix <- function(x = matrix()) {
	inversemx <- NULL
	set <- function(y) {
        x <<- y                 # the <<-symbol is used instead of the <- symbol in order to be able to access an element (x) which is defined outside of the current function (set).  
        inversemx <<- NULL
    }
	get <- function() x
	setinversemx <- function(anotherinversemx) {
		inversemx <<- anotherinversemx
	}
	getinversemx <- function() inversemx
	list(set = set, get = get, setinversemx = setinversemx, getinversemx = getinversemx)
}

# The function cacheSolve creates the inverse of the given environment (which was created by makeCacheMatrix); 
# if the inverse is already present in the environment, the function returns it. If there is no inverse in the environment, it calculated it again and writes it to the environment.

cacheSolve <- function(matrixenvironment, ...) {
        ## Return a matrix that is the inverse of 'matrixenvironment'
		if(!is.null(matrixenvironment$getinversemx())) {
			return(matrixenvironment$getinversemx())
		}
		solvedmx <- solve(matrixenvironment$get())
		matrixenvironment$setinversemx(solvedmx)
		return(solvedmx)
}