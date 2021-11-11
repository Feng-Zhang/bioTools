# rm(list=ls());options(stringsAsFactors = F)
# ### 超全英文版参考资料：
# #https://link.zhihu.com/?target=https%3A//haroldpimentel.wordpress.com/2014/05/08/what-the-fpkm-a-review-rna-seq-expression-units/
#
# ### FPKM确实存在不准确性，推荐使用TPM
# ### read count和FPKM结果都可以转成TPM，但是因为FPKM跟TPM的计算都考虑了基因长度，所以从FPKM转TPM最方便快捷
# #################### FPKM转换成TPM ############################
# fpkmToTpm <- function(fpkm){
#   exp(log(fpkm) - log(sum(fpkm)) + log(1e6))
# }
#
#
#
# ##################### Counts转TPM ###########################
# countToTpm <- function(counts, effLen){
#   rate <- log(counts) - log(effLen)
#   denom <- log(sum(exp(rate)))
#   exp(rate - denom + log(1e6))
# }
#
# ##################### Counts转FPKM ##########################
# countToFpkm <- function(counts, effLen){
#   N <- sum(counts)
#   exp( log(counts) + log(1e9) - log(effLen) - log(N) )
# }
#
# #################### Counts转Effective counts ##################
# Counts2EffCounts <- function(counts, len, effLen){
#   counts * (len / effLen)
# }
#
# # An example
# ######## count convert
# cnts <- c(4250, 3300, 200, 1750, 50, 0)
# lens <- c(900, 1020, 2000, 770, 3000, 1777)
# countDf <- data.frame(count = cnts, length = lens)
# #effect length 的计算，即校正实验误差后的序列长度，同时由effect length 产生effect counts，
# #为了方便理解，此处把原始数据当成effect并进行后续计算，详细见下方英文文章说明
# ## assume a mean(FLD) = 203.7
# #countDf$effLength <- countDf$length - 203.7 + 1
# countDf$effLength=countDf$length
# countDf$tpm <- countToTpm(countDf$count, countDf$effLength)
# countDf$fpkm <- countToFpkm(countDf$count, countDf$effLength)
#
# ######### fpkmToTpm
# tpms <- apply(expMatrix,2,fpkmToTpm)
# tpms[1:3,]
# #最后可以根据TPM的特征进行检查，看每列加和是否一致
# colSums(tpms)
# TPMs <- apply(exp,2,fpkmToTpm)
#
