##The first function, makeCacheMatrix creates a special "Matrix", 
##which is really a list containing a function to do the below 
##1- set the value of the Matrix
##2- get the value of the Matrix
##3- set the value of the Inverse of Matrix
##4- get the value of the Inverse of Matrix

makeCacheMatrix <- function(x=matrix()) {

        ## Initialize Matrix Invese Variable Inv by NULL
        inv<-NULL

        ## This function is used to get the value of the Matrix
        get<-function()x

        ## This function is used to set the value of the Matrix to new value
        ## It also Initialize Inverse of Matrix to null
        set<-function(newmat)
        {
                x<<-newmat
                inv<<-NULL
        }

        ## This function get the value of the Inverse of the Matrix
        getinv<-function(){
                return(inv)
        }

	## This function is used to set new Matrix Inverse Value
        setinv<-function(newinv)
        {
                inv<<-newinv
        }
        
        list(get=get,set=set,getinv=getinv,setinv=setinv)
}



##cacheSolve function is used to calculate the inverse of the special matrix ##created by the above function [makeCacheMatrix], it first checks if the ##inverse of the matrix already calculated, if check result means its already ##calculated then it return the inverse of the matrix from cache else it ##calculate the inverse of matrix using function [ solve ] then it sets the ##value of the inverse matrix in the case using the setinv function in the ##makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Get current Value of Inverse of Matrix
        InvResult<-x$getinv();
	## Check if Returned Inv Matrix is not null
        if(!is.null(InvResult)) {
                message("Getting Cached Inverted Matrix Data")
                return(InvResult)
        }

## Calculate the inverse of Matrix using Solve Function
InvResult<-solve(x$get())
## Set Inverse of Matrix to cache
x$setinv(InvResult)
InvResult
}