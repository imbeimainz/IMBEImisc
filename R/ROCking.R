library(ROCR)
library(Epi)
library(OptimalCutpoints)
library(pROC)

#' Title
#'
#' @param pred 
#'
#' @return
#' @export
#'
#' @examples
youden_rocr <- function(pred) {
  sensis <- performance(pred,measure = "sens")@y.values[[1]]
  specis <- performance(pred,measure = "spec")@y.values[[1]]
  summing <- sensis + specis
  youden_ind <- which.max(summing)
  cutoffs <- performance(pred,measure = "sens")@x.values[[1]]
  youden_val <- cutoffs[youden_ind]
  sensi_yv <- performance(pred,measure = "sens")@y.values[[1]][youden_ind]
  speci_yv <- performance(pred,measure = "spec")@y.values[[1]][youden_ind]
  ppv_yv <- performance(pred,measure = "ppv")@y.values[[1]][youden_ind]
  npv_yv <- performance(pred,measure = "npv")@y.values[[1]][youden_ind]
  return(
    list(
      YoudenValue = youden_val,
      SENSI_yv = sensi_yv,
      SPECI_yv = speci_yv,
      PPV_yv = ppv_yv,
      NPV_yv = npv_yv)
  )
}

#' Title
#'
#' @param df 
#' @param predictions_name 
#' @param outcomes_name 
#' @param decorate_youden 
#' @param decorate_performance 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' # Using the basic example from the ROCR package
#' library(ROCR)
#' data(ROCR.simple)
#' pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
#' perf <- performance( pred, "tpr", "fpr" )
#' plot( perf, main ="made with default plot function from ROCR" )
#' 
#' rocplot(ROCR.simple,"predictions","labels", main = "made with rocplot")
rocplot <- function(df, predictions_name, outcomes_name, 
                    decorate_youden = TRUE, decorate_performance = TRUE,
                    ...) {
  
  pred <- ROCR::prediction(df[[predictions_name]],df[[outcomes_name]])
  rocperf = performance(pred, measure = "tpr", x.measure = "fpr")
  plot(rocperf,lwd = 2,
       xlab = "FPR (1 - specificity)",
       ylab = "TPR (sensitivity)",...)
  abline(a=0, b= 1,lwd=0.5)
  my_youden <- youden_rocr(pred)
  my_auc <- performance(pred, "auc")@y.values[[1]]
  
  # decorating with some infos
  crn <- par()$usr
  if(decorate_youden) {
    points(1-my_youden$SPECI_yv,my_youden$SENSI_yv,col="steelblue",pch= 20)
    text(1-my_youden$SPECI_yv,my_youden$SENSI_yv,col="steelblue",
         labels = paste0("cutoff value (Youden Index): ",my_youden$YoudenValue),
         adj = c(-0.05,1),cex = 0.7)
  }
  
  if(decorate_performance) { 
    text(0.75 * crn[2] + 0.05 * crn[1], 0.15,
         labels = paste0(
           "Performance measures (YI)",
           "\nSensitivity: ", formatC(100 * my_youden$SENSI_yv, digits = 2, format = "f"), 
           "%\nSpecificity: ", formatC(100 * my_youden$SPECI_yv, digits = 2,format = "f"),
           "%\nPPV: ", formatC(100 * my_youden$PPV_yv, digits = 2, format = "f"),
           "%\nNPV: ", formatC(100 * my_youden$NPV_yv, digits = 2,format = "f"),
           "%\n\nAUC: ", formatC(my_auc, digits = 3,format = "f"),
           sep = ""),
         col="steelblue",cex = 0.7)
  }
  return(
    invisible(
      list(
        pred = pred,
        YoudenPerf = my_youden,
        AUC = my_auc
      )))
}  
