##' @title Clean the pheontype and expression data from TCGA
##' @details nothing
##' @param phe A data.frame, the raw clinical information including vital_status, days_to_last_follow_up and days_to_death columns.
##' @param expr A data.frame, the raw expression matrix with colname of molecular id and individual ids.
##' @param min_survival_time integer, the threshold to delete individual with survival time < 30 days
##' @return A list includes clinical information and expression matrix
##' @examples
##' \dontrun{
##' data(phe);data(expr)
##' pheExpr = create_tcga(phe,expr)
##' }
##' @export
create_tcga <- function(phe,expr,min_survival_time=30){
  phe <- clean_phe(phe,min_survival_time=min_survival_time)
  expr <- split_expr(expr)
  common_id <- intersect(phe$bcr_patient_barcode,colnames(expr$tumor_expr))
  phe <- phe[match(common_id,phe$bcr_patient_barcode),]
  expr$tumor_expr <- expr$tumor_expr[,common_id]
  expr$tumor_expr_id <- expr$tumor_expr_id[match(common_id,expr$tumor_expr_id$bcr_patient_barcode),]
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
  phe[,"survival_status"] <- as.numeric(phe[,"vital_status"]=="Dead")
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
##' \dontrun{
##' data(expr);expr = split_expr(expr)
##' }
split_expr <- function(expr){
  stopifnot(str_detect(colnames(expr)[1],pattern="TCGA-.{2}-.{4}-.{3}.*"))
  stopifnot(class(expr)[1]=="data.frame")
  colnames(expr) <- str_sub(colnames(expr),1,16)

  # delete deplicated individual
  if(any(duplicated(colnames(expr)))){
    expr <- expr[,unique_tcga_col(colnames(expr))]
  }

  # extract tumor and duplicate
  tcga_expr <- list(all_expr=expr,tumor_expr=NULL,normal_expr=NULL,tumor_expr_id=NULL,normal_expr_id=NULL)
  tcga_expr$tumor_expr <- expr[,str_sub(colnames(expr),14,15) != "11"]
  tcga_expr$tumor_expr_id <- data.frame(tcga_id=colnames(tcga_expr$tumor_expr),
                                        bcr_patient_barcode=str_sub(colnames(tcga_expr$tumor_expr),1,12),
                                        type=str_sub(colnames(tcga_expr$tumor_expr),14,15))
  colnames(tcga_expr$tumor_expr) <- tcga_expr$tumor_expr_id$bcr_patient_barcode
  if(any(duplicated(tcga_expr$tumor_expr_id$bcr_patient_barcode))){
    unique_idx <- unique_tcga_col(tcga_expr$tumor_expr_id$bcr_patient_barcode,type=tcga_expr$tumor_expr_id$type)
    tcga_expr$tumor_expr <- tcga_expr$tumor_expr[,unique_idx]
    tcga_expr$tumor_expr_id <- tcga_expr$tumor_expr_id[unique_idx,]
  }

  # extract normal and duplicate
  tcga_expr$normal_expr <- expr[,str_sub(colnames(expr),14,15)=="11"]
  tcga_expr$normal_expr_id <- data.frame(tcga_id=colnames(tcga_expr$normal_expr),
                                         bcr_patient_barcode=str_sub(colnames(tcga_expr$normal_expr),1,12),
                                         type=str_sub(colnames(tcga_expr$normal_expr),14,15))
  colnames(tcga_expr$normal_expr) <- tcga_expr$normal_expr_id$bcr_patient_barcode
  if(any(duplicated(tcga_expr$normal_expr_id$bcr_patient_barcode))){
    unique_idx <- unique_tcga_col(tcga_expr$normal_expr_id$bcr_patient_barcode,type=tcga_expr$normal_expr_id$type)
    tcga_expr$normal_expr <- tcga_expr$normal_expr[,unique_idx]
    tcga_expr$normal_expr_id <- tcga_expr$normal_expr_id[unique_idx,]
  }

  return(tcga_expr)
}

unique_tcga_col <- function(tcga_id,type=NULL){
  stopifnot(str_detect(tcga_id,pattern="TCGA-.{2}-.{4}.*"))
  if(is.null(type)){
    stopifnot(str_detect(tcga_id,pattern="TCGA-.{2}-.{4}-.{3}.*"))
    type <- str_sub(tcga_id,14,15)
  }
  ids <- data.frame(sample=tcga_id, type=type)
  uniqIds <- by(ids,ids$sample,function(x) row.names(x)[which.min(x$type)])
  return(as.numeric(uniqIds))
}

# unique_pos <- function(expr, MARGIN=1, obj="tcga"){
#   stopifnot(class(expr)[1]=="data.frame")
#   if(MARGIN==1){
#     uniqIds
#   } else if(MARGIN==2){
#     ids <- data.frame(sample=colnames(expr),type = str_sub(colnames(expr),14,15))
#     uniqIds <- by(ids,ids$sample,function(x) row.names(x)[which.min(x$type)])
#   } else {
#     stop("MARGIN is either 1 or 2")
#   }
#   return(uniqIds)
# }

