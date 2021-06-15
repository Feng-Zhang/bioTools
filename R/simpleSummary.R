##' @title simpleSummary
##' @description This function would extract tidy information
##' @param summaryObj The summary class
##' @return a data.frame
##' @export
simpleSummary = function(summaryObj){
  UseMethod('simpleSummary')
}
#' @rdname simpleSummary
#' @export
simpleSummary.default = function(summaryObj){
  summaryObj
}
#' @rdname simpleSummary
#' @export
simpleSummary.summary.coxph = function(summaryObj){
  c(HR=summaryObj$conf.int[,"exp(coef)"],
    HR.95L=summaryObj$conf.int[,"lower .95"],
    HR.95H=summaryObj$conf.int[,"upper .95"],
    coxPvalue=summaryObj$coefficients[,"Pr(>|z|)"])
}
