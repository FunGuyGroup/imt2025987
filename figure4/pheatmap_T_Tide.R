inf1="input_cor.txt"
inf2="input_p.txt"

library(pheatmap)
data=read.table(inf1,sep="\t",quote="",header=T,stringsAsFactors=F,check.names=F,row.names=1)
data=t(data)
datap=read.table(inf2,sep="\t",quote="",header=T,stringsAsFactors=F,check.names=F,row.names=1)
datap_format<-datap
#datap_format[is.na(datap_format)]<-0
datap_format=t(datap_format)
datap_format=datap_format[rownames(data),colnames(data)]
removeRowAllNa<-function(x){x[apply(x, 1,function(y){any(!is.na(y))}),]} #删除行
removeColAllNa<-function(x){x[,apply(x, 2,function(y){any(!is.na(y))})]} #删除列
data1=removeRowAllNa(data)
data1=t(removeColAllNa(data1))
datap_format=removeRowAllNa(datap_format)
datap_format=t(removeColAllNa(datap_format))
#使用显著性星号标记进行替换；
datap_format[datap_format>=0 & datap_format < 0.001] <- "***"
datap_format[datap_format>=0.001 & datap_format < 0.01] <- "**"
datap_format[datap_format>=0.01 & datap_format < 0.05] <- "*"
datap_format[datap_format>=0.05 & datap_format <= 1] <- ""
#aa<-pheatmap(as.matrix(data1),fontsize_number=5,display_numbers=datap_format,clustering_method="ward.D2",color=colorRampPalette(c("blue","white","red"))(100),cluster_rows=T,cluster_cols=T,treeheight_col=50,fontsize_row=1,fontsize_col=8,border=FALSE,show_rownames=F)
aa<-pheatmap(t(data1),display_numbers=t(datap_format),cellwidth=20,cellheight=20,fontsize_number=10,clustering_method="ward.D2",color=colorRampPalette(c("#93B7CE","white","#D51D1F"))(100),cluster_rows=F,cluster_cols=F,treeheight_col=50,fontsize_row=8,fontsize_col=8,border=FALSE)