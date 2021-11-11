# library("TxDb.Mmusculus.UCSC.mm10.knownGene") #该函储存转录本信息，包括基因的外显子信息
# txdb <- TxDb.Mmusculus.UCSC.mm10.knownGene
# #人
# library("TxDb.Hsapiens.UCSC.hg19.knownGene")
# txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
#
# if(F){
#   exon_txdb=exons(txdb) #包含了所有外显子的坐标和外显子编码id
#   exon_txdb
#   genes_txdb=genes(txdb) #包含了所有基因的坐标和基因ID
#   genes_txdb
#   o = findOverlaps(exon_txdb,genes_txdb) #找到二者之间的交叉
#   o #找到exon的第18个和gene的第6909个对应
#   #  queryHits subjectHits
#   #  <integer>   <integer>
#   #    [1]        18        6909
#   #  [2]        19        6909
#   #  [3]        20        6909
#   #  [4]        21        6909
#
#   t1=exon_txdb[queryHits(o)] #将所有对应上的外显子信息提取出来
#   t2=genes_txdb[subjectHits(o)] #将所有基因信息提取出来
#   t1=as.data.frame(t1)
#   t1$geneid=mcols(t2)[,1] #mcols就是取t2的gene_id,给ti加上一列gene_id
#
#   #lapply : 遍历列表向量内的每个元素，并且使用指定函数来对其元素进行处理。返回列表向量。
#   #函数split()可以按照分组因子，把向量，矩阵和数据框进行适当的分组;
#   #它的返回值是一个列表，代表分组变量每个水平的观测。
#   g_l = lapply(split(t1,t1$geneid),function(x){
#     # x=split(t1,t1$geneid)[[1]]
#     head(x)
#     tmp=apply(x,1,function(y){
#       y[2]:y[3]
#     }) #按行将一个基因中所有外显子的坐标点全部展示出来
#     length(unique(unlist(tmp))) #对所有坐标点进行去重复的操作，去除外显子坐标存在重合的问题
#     # sum(x[,4])
#   })
#   head(g_l)
#   g_l=data.frame(gene_id=names(g_l),length=as.numeric(g_l))
#
#   save(g_l,file = 'step7-g_l.Rdata')
# }
# load(file = 'step7-g_l.Rdata') #基因编号和对应外显子的长度
#
# head(g_l)
# library(org.Mm.eg.db) #包含gene_id的symbol  小鼠
# s2g=toTable(org.Mm.egSYMBOL)
# head(s2g)
# g_l=merge(g_l,s2g,by='gene_id') #把g_l,s2g两个数据框以'gene_id'为连接进行拼接
#
#
#
#
#
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("GenomicFeatures")
# ## 加载R包
# library("GenomicFeatures")
# ## 导入gff3文件
# txdb <- makeTxDbFromGFF("genome.gff3",format="gff3")
# ## 获取外显子位置
# exons_gene <- exonsBy(txdb, by = "gene")
# ## 去除外显子重叠部分，计算外显子长度
# exons_gene_len <- lapply(exons_gene,function(x){sum(width(reduce(x)))})
