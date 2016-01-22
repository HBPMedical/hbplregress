#' Joins aggregates from 'N' nodes to compute a linear regression. We have to compute beta from the linear equation:
#' y = A*beta + error, but data y is distributed over local nodes.
#'
#' @param betas regression coefficients computed in different 'N' nodes (list data type).
#' @param Sigmas Covariance matrices of the regression coefficients in different 'N' nodes (list data type).
#' @param Ndegree Number of degree of freedom.
#' @return betaf  : Regression coefficient joining properly betas and Sigmas from 'N' nodes.
#'                  This will be the result the user will receive. The intermediate betas are not shown.
#'         Sigmaf : Covariance matrix of joining properly the partial covariance matrices coming from different nodes.
#'         t      : T-students
#'         pvals  : p-values, significance of the regression coefficients.
#' @keywords regression
#' @export
LRegress_Federation <- function(betas,Sigmas,Ndegree){
  # Lester Melie-Garcia
  # LREN, CHUV.
  # Lausanne, September 11th, 2015

  N <- length(betas); # Number of nodes
  Sigmaf <- matrix(0,length(Sigmas[[1]][,1]),length(Sigmas[[1]][1,]));
  pbetai <- matrix(0,length(betas[[1]]),1);
  for (i in 1:N) {
    invSigmai <- MASS::ginv(Sigmas[[i]]);
    pbetai <- pbetai + invSigmai%*%betas[[i]];
    Sigmaf <- Sigmaf + invSigmai;
  }

  Sigmaf <- MASS::ginv(Sigmaf);
  betaf <- Sigmaf%*%pbetai;

  t <- betaf/sqrt(diag(Sigmaf));
  pvals <- dt(t,Ndegree);

  rout <- list(coefficients=betaf, residuals=Sigmaf,t,pvals);

  return(rout);
}
