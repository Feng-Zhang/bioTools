##' @title simple_summary
##' @description This function would extract tidy information
##' @param summary_obj The summary class
##' @return a data.frame
##' @export
simple_summary <- function(summary_obj){
  UseMethod('simple_summary')
}
#' @rdname simple_summary
#' @export
simple_summary.default <- function(summary_obj){
  summary_obj
}
#' @rdname simple_summary
#' @export
simple_summary.summary.coxph <- function(summary_obj){
  c(HR=summary_obj$conf.int[,"exp(coef)"],
    HR_95L=summary_obj$conf.int[,"lower .95"],
    HR_95H=summary_obj$conf.int[,"upper .95"],
    cox_pvalue=summary_obj$coefficients[,"Pr(>|z|)"])
}
