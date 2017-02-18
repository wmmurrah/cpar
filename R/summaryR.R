#' summaryR
#' @param model from lm()
#' @param type see hccm() in car package
#' @author John Fox, tony91782 <https://gist.github.com/tony91782/954316>
#' @export
summaryR <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
  ## ---------------------------------------------------------------------------------------- ##
  ## Author: John Fox ##
  ## Source: http://r.789695.n4.nabble.com/R-extend-summary-lm-for-hccm-td815004.html ##
  ## Adapted by Tony Cookson. ##
  ## -- Only Change Made: Changed the name of the function (unwisely maybe) ##
  ## to summaryR from summaryHCCM.lm. I also changed the spelling of consistent ##
  ## ---------------------------------------------------------------------------------------- ##

  if (!require(car)) stop("Required car package is missing.")

  type <- match.arg(type)
  V <- hccm(model, type=type)
  sumry <- summary(model)
  table <- coef(sumry)
  table[,2] <- sqrt(diag(V))
  table[,3] <- table[,1]/table[,2]
  table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)

  sumry$coefficients <- table
  p <- nrow(table)
  hyp <- cbind(0, diag(p - 1))
  sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
  sumry$type <- type

  return(sumry)


}
