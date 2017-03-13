#' Create new R script
#' @param filename Name of new script file
#' @param author Name of author, default is "William Murrah"
#' @param subdir Subdirectory to place file, default, R/
#' @param open logical, whether to open script, default TRUE
#' @export
#'
newR <- function(filename,
         author = "William Murrah",
         subdir="R/", open=TRUE) {
  lines = '#**************************************************************************
# Title: function.name
# Author: author.name
# Description:
# Created: Sys.time
# R version: R.version.string
# Directory: getwd
#**************************************************************************
# packages used -----------------------------------------------------------
  '
  filepath <- paste0(subdir, filename)
  #lines = readLines(template)
  lines = sub('author.name', author, lines)
  lines = sub('function.name', filename, lines)
  lines = sub('Sys.time', format(Sys.time(), '%A, %d %B %Y'), lines)
  lines = sub('getwd', getwd(), lines)
  lines = sub('R.version.string', R.version.string, lines)
  writeLines(lines, filepath)
  if (open) file.edit(filepath)
  filepath
}

