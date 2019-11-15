

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



#' plot_pkg
#' 
#' A function to check and compare the "poplarity" of R-packages.
#' It uses the cranlogs-package to get download counts, which are then plotted over time.
#' *to be extended* 
#' 
#' @param pkg_names character string of package names to compare
#' @param start_date character in ISO-format, default is "2012-01-01" 
#' @param ... 
#'
#' @return a ggplot-object
#' @export
#'
#' @examples
#' require(ggplot2)
#' plot_pkg(c("gee","geepack"))
#' 
#' 
plot_pkg <- function(pkg_names, start_date="2012-01-01", log_scale=FALSE...){
  if(!"cranlogs" %in% rownames(installed.packages())){
    install.packages("cranlogs")
  }
  Counts <- data.frame()
  for(i in 1:length(pkg_names)){
    Counts <- rbind(Counts,cranlogs::cran_downloads(pkg_names[i], from=start_date))
  }
  plt <- ggplot(data=Counts,aes(date,count,col=package)) + geom_smooth() + geom_point(alpha=.1)
  if(log_scale) plt <- plt + scale_y_log10()
  return(plt)
}
