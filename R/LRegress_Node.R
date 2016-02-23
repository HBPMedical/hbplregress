
#' Computes the linear regression.  We have to compute beta from the linear equation: y = A*beta + error
#' This is the function that will be running at local node level. The data (input parameters: y, A) are obtained from the
#' local databases using a specific queries. These queries will be the same for all nodes.
#'
#' @param yA Data obtained from specific query to the local Database.
#' @param ycolumn Name of the variable (column) that will dependent variable (left hand of the Linear model equation).
#' @param Acolumns List of names of the co-variables for the design or regression matrix that will be obtained  from specific query to the local Database.
#' @param Agroups  List of names of the variables to group, default to empty list
#' @return betai  (rout[1]) : regression coefficient computed in nodes 'i'.
#'         Sigmai (rout[2]) : Covariance matrix of the regression coefficients betai.
#'                 rout[3]  : Summary of the linear regression results.
#' @keywords regression
#' @export
LRegress_Node <- function(yA, ycolumn, Acolumns, Agroups) {
  # Lester Melie-Garcia
  # LREN, CHUV.
  # Lausanne, June 24th, 2015

  if (missing(Agroups)) {
      Agroups <- c();
  }

  # Convert all strings to factors
  yA[sapply(yA, is.character)] <- lapply(yA[sapply(yA, is.character)], as.factor);

  # Constructing the linear model sentence ...
  covarsmodel <- paste(Acolumns, collapse="+");
  groupsmodel <- paste(Agroups, collapse=":");
  smodel <- paste(c(groupsmodel,covarsmodel), collapse="+");

  smodelf <- paste(ycolumn," ~ ",smodel,sep = '');

  lm_out <- lm(smodelf, data = yA);

  betai <- lm_out$coefficients;

  Sigmai <- vcov(lm_out);

  Anova <- NA;

  if (len(Agroups) > 1) {
     Anova <- try(anova(lm_out));
     if (class(Anova) == "try-error") {
       cat("Cannot perform Anova: ", Anova);
       Anova <- NA;
     }
  }

  rout <- list(coefficients=betai, residuals=Sigmai, summary=summary(lm_out), anova=Anova);

  return(rout);
}
