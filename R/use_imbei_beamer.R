#' use_imbei_beamer
#' 
#' compact descr
#' 
#' some more details
#'
#' @param keep_tex Logical, fffd
#' @param copy_to Path whehejhhe
#'
#' @return Creates a tex and prints out the suggested yaml content
#' @export
#'
#' @examples
#' use_imbei_beamer()
use_imbei_beamer <- function(keep_tex = TRUE, copy_to = getwd()) {
  # copy the template from IMBEImisc to the current location
  invisible(
    file.copy(
      from = system.file("rmarkdown/templates/IMBEI_beamer/skeleton",
                         "IMBEI_beamer_template.tex",package = "IMBEImisc"),
      to = copy_to
    )
  )
  invisible(
      file.copy(
      from = system.file("rmarkdown/templates/IMBEI_beamer/skeleton",
                         "Universitaetsmedizin.jpg",package = "IMBEImisc"),
      to = copy_to
    )
  )
  
  
  # suggest yaml added lines
  message("\nCopy these lines into the YAML header of your .Rmd file")
  
  message(
    paste0(
    '
---\ntitle: "Your Title"\nauthor: "Your name"\ndate: "Date"\noutput:  \n  beamer_presentation:\n    template: IMBEI_beamer_template.tex\n    keep_tex: ', keep_tex,'\n---
    '
    )
  )
  
  
  
}
