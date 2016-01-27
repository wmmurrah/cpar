#' Creates an treatment on the treated model with the ivreg function from
#' the AER package.
#' @param y Character vector of length = 1 that is the column name of the
#'         outcome variable.
#' @param tr Character vector of length = 1 that is the column name of the
#'         treatment received variable.
#' @param iv   Character vector of length = 1 that is the column name of the
#'         insrumental variable (e.g. treatment assignment).
#' @param x Character vector of any length that contains the column names of
#'         the covariates.
#' @param data The data frame containing the variables referenced by the previous
#'         arguments.
#' @author William Murrah
#' @export
#' @return An object of class 'ivreg' containing elements of model objects similar
#'   to the lm() function.
totReg  <- function(y=outcome,iv=instrument, tr=treatment, x=covariates,
                    data=df, ...){
  # Creates an treatment on the treated model with the ivreg function from
  # the AER package.
  #
  # Args:
  #
  #   tr:
  #   iv:   Character vector of length = 1 that is the column name of the
  #         insrumental variable (e.g. treatment assignment).
  #   x:    Character vector of any length that contains the column names of
  #         the covariates.
  #   data: The data frame containing the variables referenced by the previous
  #         arguments.
  # Returns:
  #
  require(AER)
  # Create formula to be passed to 'ivreg'.
  fml <- as.formula(paste('scale(',y,')', ' ~ ',tr, ' + ',
                          paste(x, collapse=' + ')))
  instruments <- as.formula(paste('~', paste(iv, collapse=' + '), ' + ',
                                  paste(x, collapse=' +')))
  # Create ivreg model object.
  mod <- ivreg(fml, instruments, data=df)
  return(mod)
}
