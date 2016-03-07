
#' Computes the linear regression.  We have to compute beta from the linear equation: y = A*beta + error
#' This is the function that will be running at local node level. The data (input parameters: y, A) are obtained from the
#' local databases using a specific queries. These queries will be the same for all nodes.
#'
#' @param data Data obtained from specific query to the local Database.
#' @param varname Name of the varname (column) that will dependent varname (left hand of the Linear model equation).
#' @param covarnames List of names of the co-varnames for the design or regression matrix that will be obtained  from specific query to the local Database.
#' @param groups  List of names of the varnames to group, default to empty list
#' @return betai  (rout[1]) : regression coefficient computed in nodes 'i'.
#'         Sigmai (rout[2]) : Covariance matrix of the regression coefficients betai.
#'                 rout[3]  : Summary of the linear regression results.
#' @keywords regression
#' @export
LRegress_Node <- function(data, varname, covarnames, groups) {
  # Lester Melie-Garcia
  # LREN, CHUV.
  # Lausanne, June 24th, 2015

  if (missing(groups)) {
      groups <- c();
  }

  # Convert all strings to factors
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor);
  # If the variable is a factor, convert it to a numeric
  data[varname] <- sapply(data[varname], function(x) {if (is.factor(x)) as.numeric(x)});

  # Constructing the linear model sentence ...
  covarsmodel <- paste(covarnames, collapse="+");
  groupsmodel <- paste(groups, collapse=":");
  cvgmodel <- c(groupsmodel,covarsmodel);
  cvgmodel <- cvgmodel[lapply(cvgmodel,nchar)>0];

  smodel <- paste(cvgmodel, collapse="+");

  smodelf <- paste(varname," ~ ",smodel,sep = '');

  lm_out <- lm(smodelf, data = data);

  betai <- lm_out$coefficients;

  Sigmai <- vcov(lm_out);

  Anova <- NA;

  Anova <- try(anova(lm_out));
  if (length(Anova) == 1 && class(Anova) == "try-error") {
    cat("Cannot perform Anova: ", Anova);
    Anova <- NA;
  }

  rout <- list(coefficients=betai, residuals=Sigmai, summary=summary.lm(lm_out), anova=Anova);

  return(rout);
}
