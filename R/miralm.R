#' Intent to treat regression
#' @param mira mira object from `mice` package
#' @param n.obs Number of observations
#' @author William Murrah
#' @export

require(texreg)
# First, create a class definition for your regression objects(midslm):
setClass(Class="miralm",
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

miralm <- function(mira,n.obs=n) {
  require(mice)
  if (!is.mira(mira))
    stop("The object must have class 'mira'")
  mod <- mira
  pmod <- pool(mod)
  spmod <- summary(pmod)
  n <- n.obs
  coef.names <- dimnames(spmod)[[1]]
  coef <- spmod[ ,1]
  se <- spmod[ ,2]
  pvalues <- spmod[ ,5]
  gof.names <- character()
  rs <- round(pool.r.squared(mod)[1],3)
  adj <- round(pool.r.squared(mod,T)[1],3)
  new("miralm", names=coef.names, coef=coef, se=se, pval=pvalues, rsq=rs,adjrs=adj, n=n)
}


# Then write an extension that translates midslm objects into texreg objects:
extract.miralm <- function(model) {
  tr <- createTexreg(
    coef.names=model@names,
    coef=model@coef,
    se=model@se,
    pvalues=model@pval,
    gof.names=c("Num. obs.", "R$^2$","Adj. R$^2$"),
    gof=c(model@n, model@rsq, model@adjrs),
    gof.decimal=c(FALSE, TRUE, TRUE)
  )
  return(tr)
}

setMethod("extract", signature=className("miralm"),
          definition = extract.miralm)
