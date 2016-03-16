
#' Computes the linear regression.  We have to compute beta from the linear equation: y = A*beta + error
#' This is the function that will be running at local node level. The data (input parameters: y, A) are obtained from the
#' local databases using a specific queries. These queries will be the same for all nodes.
#'
#' @param data Data obtained from specific query to the local Database.
#' @param variable Name of the variable (column) that will be the dependent variable (left hand of the Linear model equation).
#' @param covariables List of names of the co-variables for the design or regression matrix that will be obtained  from specific query to the local Database.
#' @param grouping  List of names of the variables to group, default to empty list
#' @return betai  (rout[1]) : regression coefficient computed in nodes 'i'.
#'         Sigmai (rout[2]) : Covariance matrix of the regression coefficients betai.
#'                 rout[3]  : Summary of the linear regression results.
#' @keywords regression
#' @export
LRegress <- function(data, variable, covariables, grouping) {
  # Lester Melie-Garcia
  # LREN, CHUV.
  # Lausanne, June 24th, 2015

  variable <- unlist(variable);
  covariables <- unlist(covariables);
  grouping <- unlist(grouping);
  if (missing(grouping)) {
      grouping <- c();
  }

  columns <- names(data);
  # Convert all strings to factors
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor);
  # If the variable is a factor, convert it to a numeric
  varcolumn <- lapply(data[c(variable)], function(x) {if (is.factor(x)) as.numeric(x) else x});
  othercolumns <- as.data.frame(data[ , !names(data) %in% c(variable)]);
  names(othercolumns) <- columns[ columns != variable ];
  data <- cbind(as.data.frame(varcolumn), othercolumns);

  # Constructing the linear model sentence ...
  covarsmodel <- paste(covariables, collapse="+");
  groupingmodel <- paste(grouping, collapse=":");
  cvgmodel <- c(groupingmodel,covarsmodel);
  cvgmodel <- cvgmodel[lapply(cvgmodel,nchar)>0];

  smodel <- paste(cvgmodel, collapse="+");

  smodelf <- paste(variable," ~ ",smodel,sep = '');

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
