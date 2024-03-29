% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROCking.R
\name{youden_rocr}
\alias{youden_rocr}
\title{Calculate performance measures at the Youden Index for a ROC curve}
\usage{
youden_rocr(pred)
}
\arguments{
\item{pred}{An object of class prediction, as commonly used by ROCR}
}
\value{
A list of values, with the Youden Index, speci, sensi, positive & negative
predictive values
}
\description{
Calculate performance measures at the Youden Index for a ROC curve
}
\examples{
# Using the basic example from the ROCR package
library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
youden_rocr(pred)   # these can be printed out or used later in plotting functions
}
