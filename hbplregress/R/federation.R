#' Joins aggregates from two nodes to compute a linear regression. We have to compute beta from the linear equation:
#' y = A*beta + error, but data y is distributed over local nodes.
#'
#' @param beta1 regression coefficients computed in nodes 1
#' @param beta2 regression coefficients computed in nodes 2
#' @param Sigma1 Covariance matrices of the regression coefficients in node 1.
#' @param Sigma2 Covariance matrices of the regression coefficients in node 2.
#' @return betaf  : Regression coefficient joining properly beta1,beta2, Sigma1 and Sigma2. 
#'                  This will be the result the user will receive. The intermediate ones beta1 and beta2 are not shown.
#'         Sigmaf : Covariance matrix of joining properly the partial covariance matrices coming from different nodes.
#' @keywords regression
#' @export
LRegress_Federation <- function(beta1,Sigma1,beta2,Sigma2){
  # Lester Melie-Garcia
  # LREN, CHUV. 
  # Lausanne, June 24th, 2015
  
  invSigma1 <- ginv(Sigma1);
  invSigma2 <- ginv(Sigma2);
  Sigmaf <- MASS::ginv(invSigma1 + invSigma2);
  betaf  <- Sigmaf%*%(invSigma1%*%beta1 + invSigma2%*%beta2);
  
  rout <- list(betaf,Sigmaf);
  return(rout);
}
