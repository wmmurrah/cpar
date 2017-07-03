#' item analysis
#' @param x x is an n subjects by k items matrix
#' @param keys is a vector with the correct keys (A, B, C, D)
#' @author Christophe Lalanne <https://gist.github.com/chlalanne>
itan <- function(x, keys=NULL, digits=3, no.resp=4) {
  stopifnot(ncol(x)>1)
  if (is.null(keys)) keys <- rep("A", ncol(x))
  require(ltm)
  raw.resp <- matrix(nrow=ncol(x), ncol=no.resp)
  colnames(raw.resp) <- LETTERS[1:no.resp]
  for (i in 1:ncol(x)) {
    tmp <- table(x[,i])
    raw.resp[i,names(tmp)] <- tmp
  }
  freq.resp <- raw.resp/apply(raw.resp, 1, sum, na.rm=T)
  na.resp <- apply(x, 2, function(x) sum(is.na(x)))
  correct.resp <- t(apply(x, 1, function(x) x==keys))
  total.score <- apply(correct.resp, 1, sum, na.rm=T)
  pbis <- apply(correct.resp, 2,
                function(x) biserial.cor(total.score, as.numeric(x),
                                         use="complete.obs"))
  nb.correct <- apply(correct.resp, 2, sum, na.rm=T)
  p.obs <- nb.correct/nrow(x)
  MC <- MI <- numeric(ncol(x))
  for (i in 1:ncol(x))
    MC[i] <- mean(total.score[correct.resp[,i]], na.rm=T)
  for (i in 1:ncol(x))
    MI[i] <- mean(total.score[!correct.resp[,i]], na.rm=T)
  raw.pc <- round(raw.resp/nrow(x), digits)

  out <- cbind(P=p.obs,R=round(pbis, digits),
               MC=round(MC, digits),MI=round(MI, digits),
               NC=nb.correct,OMIT=na.resp,raw.pc)
  return(out)
}
