#'mplus_get_file_dataset
#' @param file
#' @param datastr
#' @author Thuy Nguyen, Muthen & Muthen
#' @export

##########################################################################
#
# mplus.get.file.dataset - supporting function for getting dataset in the file
#
# arguments:
#	file - the quoted name of an existing GH5 file
#   datastr - the name of the attribute
#
# eg. mplus.get.file.dataset('ex8.1.gh5','model_group_labels')
#
mplus_get_file_dataset <- function(file, datastr) {
  if ( !(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  gh5 <- h5dump(file, load=TRUE)

  fid <- H5Fopen(file)
  dtid <- H5Dopen(fid, datastr)

  data <- H5Dread(dtid)

  H5Dclose(dtid)
  H5Fclose(fid)

  return(data)
}

estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}
