#' Intent to treat regression
#' @param target Mplus output file, with suffix .out
#' @author William Murrah
#' @export



require(texreg)
# First, create a class definition for your regression objects(.out):
setClass(Class="mpluslm",
         representation=representation(
           names="character",
           coef="numeric",
           se="numeric",
           pval="numeric",
           rsq="numeric",
           adjrs="numeric",
           n="numeric"
         )
)

mpluslm <- function(target) {
  require(MplusAutomation)
  mod <- readModels(target=target,recursive=FALSE)
  int.plc <- which(mod$parameters$unstandardized$paramHeader == "Intercepts")
  intercept <- mod$parameters$unstandardized$est[int.plc]
  int.se   <-  mod$parameters$unstandardized$se[int.plc]
  int.pval <-  mod$parameters$unstandardized$pval[int.plc]
  estimator <- mod$summaries$Estimator
  n <- mod$summaries$Observations
  k<- (length(unique(mod$parameters$unstandardized$param))-1)
  par.tn <- mod$summaries$Parameters
  coef.names <- c("(Intercept)",mod$parameters$unstandardized$param[1:k])
  coef <- c(intercept,mod$parameters$unstandardized$est[1:k])
  se <- c(int.se,mod$parameters$unstandardized$se[1:k])
  pvalues <- c(int.pval,mod$parameters$unstandardized$pval[1:k])
  gof.names <- character()
  rs <- (1 - mod$parameters$stdyx.standardized$est[par.tn])
  adj <- (rs - (k/(n-1)))
  ll <- mod$summaries$LL
  llu <- mod$summaries$UnrestrictedLL
  aic <- mod$summaries$AIC
  bic <- mod$summaries$BIC
  new("mpluslm", names=coef.names, coef=coef, se=se, pval=pvalues, rsq=rs,adjrs=adj, n=n)
}

# extract.mpluslm
extract.mpluslm <- function(model) {
  tr <- createTexreg(
    coef.names=model@names,
    coef=model@coef,
    se=model@se,
    pvalues=model@pval,
    gof.names=c("Num obs.", "R^2","Adj. R^2"),
    gof=c(model@n, model@rsq, model@adjrs),
    gof.decimal=c(FALSE, TRUE, TRUE)
  )
  return(tr)
}

setMethod("extract", signature=className("mpluslm"),
          definition = extract.mpluslm)
