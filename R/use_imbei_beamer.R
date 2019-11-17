#' use_imbei_beamer
#' 
#' Facilitates the use of the IMBEI beamer template. 
#' This is an alternative to creating a new markdown from template directly. 
#' Latter is preferred.
#'
#' @param keep_tex Logical, defines wheter the tex file is kept. Defaults to TRUE
#' @param copy_to File path for beamer template. Defaults to getwd()
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
