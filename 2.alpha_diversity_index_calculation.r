#test
rm(list=ls())
library(vegan)
library(ape)
library(picante)
input='step2.even.5000.feature-table.txt'
df<-read.delim(input,header = T,row.names = 1,check.names=F)
Shannon<-diversity(df, index = "shannon", MARGIN = 2)
Simpson<-diversity(df, index = "simpson", MARGIN = 2)
Richness <- specnumber(df,MARGIN = 2) #spe.rich == sobs
index<-as.data.frame(cbind(Shannon,Simpson,Richness))
tdf<-t(df)
tdf<-ceiling(as.data.frame(t(df)))
obs_chao_ace<-t(estimateR(tdf))
obs_chao_ace<-obs_chao_ace[rownames(index),]
index$Chao1<-obs_chao_ace[,2]
index$Ace<-obs_chao_ace[,4]
index$Sobs<-obs_chao_ace[,1]
write.table(cbind(sample=c(rownames(index)),index),'step3.diversity.index.txt',  row.names = F,sep = '\t',quote = F)



tree <- read.tree('tree.nwk')
PD_whole_tree <- pd(tdf, tree, include.root = FALSE)[1]
names(PD_whole_tree) <- 'PD_whole_tree'
index$PD_whole_tree <- PD_whole_tree$PD_whole_tree
write.table(cbind(sample=c(rownames(index)),index),'step3.diversity.index.txt',  row.names = F,sep = '\t',quote = F)

