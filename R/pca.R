##' @title plot PCA
##' @param mat A data.frame, the expression matrix in which column is multiple variables and row is individual.
##' @param phe A data.frame, the phenotype information.
##' @param group A character, one colname of phe variable.
##' @param ntop A numeric, the top number of gene to performs a principal components analysis
##' @param scale A logic, default value is FALSE, but if mat is not be normalized, it should be TRUE.
##' @return A ggplot object
##' @examples
##' \dontrun{
##' library(bladderbatch)
##' data(bladderdata)
##' dat <- bladderEset[1:1000,]
##' pheno <- pData(dat)
##' edata <- exprs(dat)
##' plot_PCA(t(edata),phe=pheno,group="cancer")
##' }
##' @export
plot_PCA <- function(mat,phe,group="condition",ntop=500,scale=FALSE){
  stopifnot(all(row.names(mat)==row.names(phe)))
  pca_promp <- pca(mat,ntop=ntop,scale = scale)
  explained_var <- pca_promp$sdev^2/sum(pca_promp$sdev^2)
  df <- data.frame(pca_promp$x,phe)
  p_pca <- gg_pca(df,group=group,explained_var=explained_var)
  return(p_pca)
}

##' @title performs a principal components analysis
##' @param mat A data.frame, the expression matrix in which column is multiple variables and row is individual.
##' @param ntop A numeric, the top number of gene to performs a principal components analysis
##' @param scale A logic, default value is FALSE, but if mat is not be normalized, it should be TRUE.
##' @return A prcomp object
##' @examples
##' \dontrun{
##' library(bladderbatch)
##' data(bladderdata)
##' dat <- bladderEset[1:1000,]
##' edata <- exprs(dat)
##' res <- pca(t(edata))
##' }
##' @export
##' @importFrom stats prcomp
##' @importFrom MatrixGenerics colVars
pca <- function(mat,ntop=500,scale=FALSE){
  rv <- colVars(mat)
  select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
  pca <- prcomp(mat[,select],scale=scale)
  return(pca)
}

##' @title plot PCA figure
##' @param df A data.frame, the expression matrix in which column is multiple variables and row is individual.
##' @param PC1 A character, the first eigenvalue
##' @param PC2 A character, the second eigenvalue
##' @param group A character, one colname of phe variable.
##' @param explained_var A numeric vector.
##' @return A ggplot object
##' @examples
##' \dontrun{
##' library(bladderbatch)
##' data(bladderdata)
##' dat <- bladderEset[1:1000,]
##' edata <- exprs(dat)
##' pheno <- pData(dat)
##' pca_promp <- pca(t(edata))
##' explained_var <- pca_promp$sdev^2/sum(pca_promp$sdev^2)
##' df <- data.frame(pca_promp$x,pheno)
##' gg_pca(df,group="cancer",explained_var=explained_var)
##' }
##' @export
##' @importFrom ggplot2 ggplot aes .data geom_point coord_fixed xlab ylab
gg_pca <- function(df,PC1="PC1",PC2="PC2",group="group",explained_var=NULL){
  stopifnot(group %in% colnames(df))
  p <- ggplot(data = df, aes(x = .data[[PC1]], y = .data[[PC2]], color = .data[[group]])) +
    geom_point(size = 3) +
    coord_fixed()

  if(!is.null(explained_var)){
    p <- p +
      xlab(paste0("PC1: ", round(explained_var[1] * 100), "% variance")) +
      ylab(paste0("PC2: ", round(explained_var[2] * 100), "% variance"))
  }
  return(p)
}
