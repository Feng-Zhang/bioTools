##' @title Clean the pheontype and expression data from TCGA
##' @details nothing
##' @param phe A data.frame, the raw clinical information including vital_status, days_to_last_follow_up and days_to_death columns.
##' @param expr A data.frame, the raw expression matrix with colname of molecular id and individual ids.
##' @param min_survival_time integer, the threshold to delete individual with survival time < 30 days
##' @return A list includes clinical information and expression matrix
##' @examples
##' \dontrun{
##' data(phe);data(expr)
##' pheExpr = creat_tcga(phe,expr)
##' }
##' @export
create_omics <- function(phe,expr,min_survival_time=30){
  phe <- clean_phe(phe,min_survival_time=min_survival_time)
  expr <- split_expr(expr)
  common_id <- intersect(phe$bcr_patient_barcode,colnames(expr$tumor_expr))
  phe <- phe[phe$bcr_patient_barcode %in% common_id,]
  expr$tumor_expr <- expr$tumor_expr[,common_id]
  expr$phe <- phe
  return(expr)
}


##' @title Clean the phenotype data from TCGA
##' @description Given a phenotype data frame, delete the individual with NA and calculate the survival time and status.
##' @details nothing
##' @param phe data.frame, the raw clinical information including vital_status, days_to_last_follow_up and days_to_death columns.
##' @param min_survival_time integer, the threshold to delete individual with survival time < 30 days
##' @return data.frame, the clean clinical information including survivalStatus and survivalTime columns.
##' @export
##' @examples
##' data(phe);phe = clean_phe(phe)
clean_phe <- function(phe,min_survival_time=30){
  #remove survial time and vital status with NA
  phe <- unique(phe[!is.na(phe[,"vital_status"]),])
  phe[,"survival_time"] <- ifelse(phe[,"vital_status"]=="Alive",phe[,"days_to_last_follow_up"],phe[,"days_to_death"])
  phe <- phe[!is.na(phe[,"survival_time"]) & phe[,"survival_time"]>min_survival_time,]
  row.names(phe) <- phe[,"bcr_patient_barcode"]

  #convert vital status to numeric
  phe[,"survival_status"] <- as.numeric(as.factor(phe[,"vital_status"]))
  return(phe)
}

##' @title Clean the expression data from TCGA
##' @description Delete the duplicated individual and rename the colnames.
##'
##' @details nothing
##' @param expr data.frame, the raw expression matrix with colname of molecular id and individual ids.
##' @return data.frame, the clean expression matrix with colname of molecular id and individual ids.
##' @importFrom stringr str_sub str_split_fixed str_detect
##' @export
##' @examples
##' data(expr);expr = split_expr(expr)
split_expr <- function(expr){
  stopifnot(str_detect(colnames(expr)[2],pattern="TCGA-.{2}-.{4}-.{3}-.{3}-.{4}-.{2}"))
  expr <- data.frame(row.names = expr[,1],expr[,-1],check.names =FALSE)
  colnames(expr) <- str_sub(colnames(expr),1,15)

  # delete deplicated individual
  if(any(duplicated(colnames(expr)))){
    ids <- data.frame(sample=colnames(expr),type = str_sub(colnames(expr),14,15))
    uniqIds <- by(ids,ids$sample,function(x) row.names(x)[which.min(x$type)])
    expr <- expr[,as.numeric(uniqIds)]
  }

  # extract tumor and normal
  tcga_expr <- list(all_expr=expr,tumor_expr=NULL,normal_expr=NULL)
  tcga_expr$tumor_expr <- expr[,str_sub(colnames(expr),14,15) != "11"]
  colnames(tcga_expr$tumor_expr) <- str_sub(colnames(tcga_expr$tumor_expr),1,12)
  tcga_expr$normal_expr <- expr[,str_sub(colnames(expr),14,15)=="11"]
  colnames(tcga_expr$normal_expr) <- str_sub(colnames(tcga_expr$normal_expr),1,12)
  return(tcga_expr)
}

