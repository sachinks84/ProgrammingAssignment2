## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix -> stores the chache of Matrix and Iverse
#cacheSolve -> compute Inverse matrix if cached value is NULL,else compute the Inverse


## cache the matrix and return a list 
makeCacheMatrix <- function(x = matrix()) {
  
  mcache <<- x;
  mInv<<-NULL;
  
  get <- function() {mcache}
  getInv<-function(){mInv}
  solveInv<-function(y) {
    mInv<<-solve(y);
    mInv
  }
  list( get = get,getInv= getInv, solve =solveInv);
  
} 


## retrun the inverse matrix from cache ,else compute 
##  and store it in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x1<-x$getInv();
  if(!is.null(x1)){
    print("returning cache..");
    return (x1);
  }else{
    
    data<-x$get();
    print("solving inv..")
    
    x2<-x$solve(data) 
    return (x2)
    
  }
  
}
