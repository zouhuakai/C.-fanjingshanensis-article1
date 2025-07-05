library(ggplot2)#install.packages("ggpubr")
library(ggpubr)
library(ggsci)
#LEfse
mydata<- read.delim('LDA_score.txt', header = T, sep = '\t',
                    stringsAsFactors = F, check.names = F)
attach(mydata)
library(ggplot2)
p=ggplot(data = mydata, mapping = aes(x=reorder(gene,LDA),y=LDA,fill=Type)) +
  geom_bar(position = "dodge",stat = "identity",color = "black",show.legend=TRUE)+
  labs(y="LDA score(log10)",x="")+#标签名
  theme(axis.title.x=element_text(vjust=2, size=20))+#调整标签字体大小
  theme(axis.text.x=element_text(vjust=2, size=20))+#调整横坐标字体大小
  scale_fill_npg()+ #图例的颜色搭配选项，具体可输入??ggsci进行查询或者上网查询
  theme(legend.position = "right")+#图例的位置
  theme(legend.key.size = unit(30, "pt"))+#图例的大小
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y =element_blank(),panel.border = element_blank(), panel.background = element_blank())+#隐藏部分内容以保证图片整洁
  coord_flip()
p
ggsave(paste("LEfse.pdf", sep=""), p, width = 13, height = 8)

