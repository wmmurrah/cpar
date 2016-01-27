#' View Mplus plots in GH5 file
#' @param file
#' @author Thuy Nguyen, Muthen & Muthen
#' @export
mplus_view_plots <- function(file) {
  ##########################################################################
  #
  # mplus.view.plots - loads the file and lists all available plots
  #
  # arguments:
  #    file - the quoted name of an existing GH5 file
  #
  # eg. mplus.view.plots('ex.gh5')
  #
  mplus.load(file)
}
