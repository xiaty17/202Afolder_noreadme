############################################################# 
## Stat 202A - Homework 7
## Author: Tianyi Xia
## Date : 2017/11/28
## Description: This script implements the lasso
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "setwd" anywhere
## in your code. If you do, I will be unable to grade your 
## work since R will attempt to change my working directory
## to one that does not exist.
#############################################################

#####################################
## Function 1: Lasso solution path ##
#####################################

myLasso <- function(X, Y, lambda_all){
  
  # Find the lasso solution path for various values of 
  # the regularization parameter lambda.
  # 
  # X: n x p matrix of explanatory variables.
  # Y: n dimensional response vector
  # lambda_all: Vector of regularization parameters. Make sure 
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns a matrix containing the lasso solution vector 
  # beta for each regularization parameter.
  
  #######################
  ## FILL IN CODE HERE ##
  #######################
  
  L=length(lambda_all)
  p=dim(X)[2]
  SS=rep(0,p+1)
  T=100
  beta_all=matrix(rep(0,(p+1)*L),nrow=(p+1))
  X=cbind(rep(1,n),X)
  for(j in 1:(p+1)){
    SS[j]=sum(X[,j]^2)
  }
  for(l in 1:L){
    lambda=lambda_all[l]
    beta=rep(0,p+1)
    for(t in 1:T){
      R=Y-X%*%beta+X[,1]*beta[1]
      beta[1]=X[,1]%*%R/SS[1]
      for(k in 2:(p+1)){
        R=Y-X%*%beta+X[,k]*beta[k]
        b=X[,k]%*%R/SS[k]
        beta[k]=sign(b)*max(0,abs(b)-lambda/SS[k])
      }
    }
    beta_all[,l]=beta
  }
  
  
  
  ## Function should output the matrix beta_all, the 
  ## solution to the lasso regression problem for all
  ## the regularization parameters. 
  ## beta_all is (p+1) x length(lambda_all)
  return(beta_all)
  
}
