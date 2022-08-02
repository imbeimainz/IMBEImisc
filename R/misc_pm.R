

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
#' A function to check and compare the "popularity" of R-packages.
#' It uses the cranlogs-package to get download counts, which are then plotted over time.
#' *to be extended* 
#' 
#' @param pkg_names character string of package names to compare
#' @param start_date character in ISO-format, default is "2012-01-01" 
#' @param log_scale logical, defines y-scale. Default is `FALSE`.
#' @param monthly logical, Should the download counts be aggregated by month? Default is `FALSE`.
#'
#' @return a ggplot-object
#' @export
#'
#' @examples
#' plot_pkg(c("gee","geepack"))
#' 
#' 
plot_pkg <- function(pkg_names, start_date="2012-01-01", log_scale=FALSE, monthly=FALSE){
  needed_pkgs  <- c("cranlogs","ggplot2")
  missing      <- !(needed_pkgs %in% rownames(installed.packages()))
  if(max(missing)>0) install.packages(needed_pkgs[missing])
  lapply(needed_pkgs,require,character.only=TRUE)

  Counts <- data.frame()
  for(i in 1:length(pkg_names)){
    Counts <- rbind(Counts,cranlogs::cran_downloads(pkg_names[i], from=start_date))
  }
  if(monthly) {
    Counts$yearmonth <- as.Date(paste0(format(Counts$date,"%Y-%m"),"-01"))
    Counts <- aggregate(count~ yearmonth + package, Counts, sum)
    names(Counts)[1] <- "date"
  }
  plt <- ggplot(data=Counts,aes(date,count,col=package)) + geom_smooth() + 
    geom_point(alpha=ifelse(monthly,.8,.1))
  if(log_scale) plt <- plt + scale_y_log10()
  return(plt)
}



#' Get correlation of paired groups from summary statistics
#' 
#' Sometimes, papers report mean and std. dev of paired groups along with a p-value,
#' but not the correlation (Often done in a typical table 1). The latter is needed
#' for sample size calculations and can be recovered with this function.
#'
#' @param meanDiff Difference of means in two paired groups 
#' @param sd1 Standard deviation in group 1
#' @param sd2 Standard deviation in group 2
#' @param pValue p value as reported in paper
#'
#' @return the correlation of the two groups
#' @export
#'
get_corr_pairedTtest <- function(meanDiff, sd1, sd2, pValue){
  t_stat <- qt(1-pValue/2,18)  ## get T statistic from p value 
  sdpool <- sqrt(19)*(meanDiff)/t_stat ## compute std dev of one-sample t-test
  Varpool <- sdpool^2
  tmp  <- (sd0^2+sd1^2) - Varpool ## Var between subjects = total var - var within subjects
  return( corr=tmp/(sd0*sd1*2) )
}
