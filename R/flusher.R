

#' Flush the content of the editor window (even if not saved)
#' 
#' Flush the content of the editor window (even if not saved), saving it into a file
#' whose name is the same as the filename in the editor, with a specified suffix
#'
#' @param out_suffix Character string, defaults to .flushed.R
#' 
#' @return Creates a file
#' @export
#'
#' @examples
#' # flush_editorcontent()     # e.g. to be inserted in the execution of a scripts
flush_editorcontent <- function(out_suffix = ".flushed.R") {
  infoset <- rstudioapi::getSourceEditorContext()
  outfile <- paste0(infoset$path,out_suffix)
  writeLines(infoset$contents,con = outfile)
}
