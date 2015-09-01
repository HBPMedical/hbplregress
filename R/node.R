
#' Computes the linear regression.  We have to compute beta from the linear equation: y = A*beta + error
#' This is the function that will be running at local node level. The data (input parameters: y, A) are obtained from the
#' local databases using a specific queries. These queries will be the same for all nodes.
#'
#' @param y Data obtained from specific query to the local Database. y : column vector on size N x 1
#' @param A Design or regression matrix that will be obtained from specific query to the local Database. A : Matrix of size N x M , M : number of regressors
#' @return betai  : regression coefficient computed in nodes 'i'.
#'         Sigmai : Covariance matrix of the regression coefficients betai.
#' @keywords regression
#' @export
LRegress_Node <- function(y,A) {
  # Lester Melie-Garcia
  # LREN, CHUV. 
  # Lausanne, June 24th, 2015
  
  At <- t(A);
  Sigmai <- MASS::ginv(At%*%A);
  betai  <- Sigmai%*%At%*%y; 
  rout   <- list(betai,Sigmai);
  return(rout);
}
