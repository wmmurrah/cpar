#' supporting function for getting dataset
#' @param file the quoted name of an existing GH5 file
#' @param groupstr the name of the group for the attribute
#' @param data sr the name of the attribute
#' @author Thuy Nguyen, Muthen & Muthen
#' @description
#' mplus.get.group.dataset('ex8.1.gh5','bayesian_data','statements')
#' @export
mplus.get.group.dataset <- function(file, groupstr, datastr) {
  if ( !(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  gh5 <- h5dump(file, load=TRUE)

  fid <- H5Fopen(file)
  gid <- H5Gopen(fid, groupstr)
  dtid <- H5Dopen(gid, datastr)

  data <- H5Dread(dtid)

  H5Dclose(dtid)
  H5Gclose(gid)
  H5Fclose(fid)

  return(data)
}

estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}
