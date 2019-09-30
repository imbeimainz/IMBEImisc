

#' print_with_confint
#'
#' this is a small auxiliary function that reformats the coefficient estimates of regression models.
#' 
#' @param coefs two-column matrix. First column should be the estimate, second column should be the the standard error as returned from summary method
#' @param df numeric, denominator degrees of freedom if t-d
#' @param exp logical, defaults to FALSE. Should the estimates be transformed with exp(.)?
#' @param conf.level numeric, confidence level. Defaults to 0.95 .
#'
#' @return a matrix 
#' @export
#'
#' @examples
#' 
#' data(datasets::mtcars)
#' mod   <- lm(mpg ~ disp+hp+wt, data=mtcars)
#' coefs <- summary(mod)$coefficients[,1:2] 
#' 
#' print_with_confint(coefs)
#' 
print_with_confint <- function(coefs,df=Inf,exp=FALSE,conf.level=.95){
  width <- abs(qt(p=(1-conf.level)/2, df=df))
  x <- cbind(coefs[,1], coefs[,1]-width*coefs[,2], coefs[,1]+width*coefs[,2])
  if(exp) x <- exp(x)
  x <- round(x,3)
  out <- as.matrix(paste0(x[,1], "  (",x[,2]," ; ",x[,3],")"))
  row.names(out) <- row.names(coefs)
  return(out)
}
