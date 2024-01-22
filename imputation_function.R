#### DATA IMPUTATION #######
# THIS FUNCTION TAKES as input a protein intensity matrix NOT log transformed
# It outputs in workspace and working directory a new matrix with missing values imputed, normalized and log2 transformed
# The function uses the impute function of MSnbase using a mixed imputation models for MAR and NMAR values
# NMAR are defined as never identified in one sample group and identified in == nrep in the other sample group
# Imputation method for MAR is 'knn' and for NMAR is 'minDet'
# For details see MSnbase manual
# OUTPUT table is returned as well written in working directory as txt file, together with summary plots for data imputation
# NEED TO DEFINE
# data: dataset matrix NOT log2 transformed
# nrep: number of replicates per sample group
# ns: number of sample groups
# maxna: maximum number of NA accepted
# name: name tag for output files
# norm.method: normalization method to be used. Options are: 'quantile' or 'median'
# AO
#########################################

data <- data_bsap
nrep <- 3  # Adjust according to your data
ns <- 2  # Adjust according to your data
maxna <- 1  # Adjust according to your preference
name <- "bsa_P_test"  # Adjust according to your preference

impute.data<-function(data,nrep,ns,maxna,name, norm.method){
  library(MSnbase)
  library(gplots)
  library(RColorBrewer)
  library(ggplot2)
  #library(Mfuzz)
  library(stringi)
  library(stringr)
  library(impute)
  library(imputeLCMD)
  library(vsn)
  
  data<-log2(data)
  
  
  
  # na count function
  NAcounts<-function(data,ns,nrep){
    NAcounts<-NULL
    p<-seq(1,(nrep*ns),by=nrep)
    for(i in 1:ns){
      tmp=apply(is.na(data[,p[i]:(p[i]+nrep-1)]),1,sum)
      NAcounts<-cbind(NAcounts,tmp)
    }
    NAcounts
  }
  
  #read in NA table (make sum of NA per sample group in a seperate file)
  na.count<-NAcounts(data,ns,nrep)
  
  pdf(paste(name, "_data_imputation_report.pdf", sep = ''))
  
  data1<-data[na.count[,1]<=maxna|na.count[,2]<=maxna,]
  #traditional complete matrix
  data1a<-data[na.count[,1]<=maxna&na.count[,2]<=maxna,]
  #extreme cases with 1 NA allowed
  #data1b<-data[na.count[,1]==nrep&na.count[,2]<=1,]
  #data1c<-data[na.count[,1]<=1&na.count[,2]==nrep,]
  #extreme cases with 0 NA allowed
  #data1b<-data[na.count[,1]==nrep&na.count[,2]<=0,]
  #data1c<-data[na.count[,1]<=0&na.count[,2]==nrep,]
  #extreme cases with 0 NA allowed and 1 max value in other group
  data1b<-data[na.count[,1]>=(nrep-1)&na.count[,2]<=0,]
  data1c<-data[na.count[,1]<=0&na.count[,2]>=(nrep-1),]
  #combined them
  data2<-rbind(data1a,data1b,data1c)
  
  na.count2<-as.data.frame(na.count[rownames(data2),])
  boxplot(data1, main= "Original data distribution")
  
  # define MNAR (Missing not at random)
  # with 1 NA allowed
  na.count2$randna<-ifelse((na.count2[,1] <=1  & na.count2[, 2] >= (nrep))|(na.count2[,1] >= (nrep)&na.count2[, 2] <= 1) ,"FALSE", "TRUE")
  # with 0 NA allowed
  #na.count2$randna<-ifelse((na.count2[,1] <=0  & na.count2[, 2] >= (nrep))|(na.count2[,1] >= (nrep)&na.count2[, 2] <= 0) ,"FALSE", "TRUE")
  # with 0 NA allowed and 1 max value in other group
  #na.count2$randna<-ifelse((na.count2[,1] <=0  & na.count2[, 2] >= (nrep-1))|(na.count2[,1] >= (nrep-1)&na.count2[, 2] <= 0) ,"FALSE", "TRUE")
  
  
  
  # number of proteins that have Missing not at random (MNAR) values (e.g 4/0, 3/0, 0/4, 0/3)
  barplot(c(nrow(na.count2[na.count2[,1] == 0 & na.count2[, 2] == 0,]),  nrow(na.count2[na.count2$randna=="TRUE",])-nrow(na.count2[na.count2[,1] == 0 & na.count2[, 2] == 0,]),
            nrow(na.count2[na.count2$randna=="FALSE",])), names.arg=c("COMPLETE","MAR","MNAR"), main="Number of proteins with missing values")
  
  # write out expression data file
  write.table(data2, "exprsFile.txt", sep="\t", quote=FALSE)
  
  # write out feature data file (it must have the same number of rows and rownames as data)
  fdata<-na.count2
  
  #write.csv(cbind(rownames(fdata),fdata), "fdataFile.csv", row.names=FALSE)
  write.table(fdata, "fdataFile.txt", sep="\t", quote=FALSE)
  
  # write out phenotype data file (it must have the same number of rows as number of columns of data and rownames as data)
  
  pdata<-data.frame(row.names=colnames(data), sample.name=colnames(data1))
  write.table(pdata, "pdataFile.txt", sep="\t", quote=FALSE)
  
  
  # generates "res" in values (this contains all the information from the 3 necessary files (expression data, feature file, phenotype file))
  res<-readMSnSet(exprsFile = "exprsFile.txt",
                  featureDataFile = "fdataFile.txt", sep="\t", header = TRUE)
  
  # you can quickly check if the data are properly "combined"
  res
  head(exprs(res))
  head(fData(res))
  head(pData(res))
  
  #correlation original data
  correlation = cor(exprs(res), method="pearson",use="pairwise.complete.obs")
  #pdf("correlation_original_data.pdf", width=11, height=11)
  mypalette<-brewer.pal(11, "PRGn")
  
  heatmap.2(correlation,trace = "none",density.info="none", cexRow=0.9, cexCol=0.9, col=mypalette,
            colsep=c(seq(1,ncol(correlation),1)),
            rowsep=c(seq(1,ncol(correlation),1)),
            sepcolor='white',sepwidth=c(0.0125,0.02), main="correlation_original_data")
  #dev.off()
  
  ###### data imputation
  
  res3 <- impute(res, method = "mixed", randna = fData(res)$randna, mar = "knn", mnar = "MinDet")
  #exprs(res)[1:7,]
  #exprs(res3)[1:7,]
  
  boxplot(exprs(res3), main= "Data distribution after imputation")
  
  if (norm.method=='quantile'){
    res.norm<-normalise(res3, "quantiles")
  }
  if (norm.method=='median'){
    res.norm<-normalise(res3, "center.median")
  }
  
  boxplot(exprs(res.norm), main= "Data distribution after imputation and normalization")
  
  
  #correlation
  correlation = cor(exprs(res3), method="pearson",use="pairwise.complete.obs")
  #pdf("correlation_after_imputation_normalization.pdf", width=11, height=11)
  mypalette<-brewer.pal(11, "PRGn")
  
  heatmap.2(correlation,trace = "none",density.info="none", cexRow=0.9, cexCol=0.9, col=mypalette,
            colsep=c(seq(1,ncol(correlation),1)),
            rowsep=c(seq(1,ncol(correlation),1)),
            sepcolor='white',sepwidth=c(0.0125,0.02), main = "correlation_after_imputation_normalization")
  #dev.off()
  
  #dev.off()
  # extract matrix normalized and with data imputed
  new.data<-as.data.frame(exprs(res.norm))
  dev.off()
  write.table(new.data, paste(name, "_imputed_normalized_values.txt", sep=""), sep="\t", quote=FALSE)
  unlink('exprsFile.txt')
  unlink('fdataFile.txt')
  unlink('pdataFile.txt')
  new.data
}
