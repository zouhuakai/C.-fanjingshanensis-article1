library(ggplot2)
library(ggsci)
library(ggpubr)
library(scales)

# 1.1 读入数据
data <- read.csv("4zu.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
data$Class2 <- factor(data$Class2, levels=data$Class2[1:41])

# 1.2 数据格式转换-宽转长
library(tidyr)
colnames(data) <- c("Class2","Class1","mean","sd","mean","sd","mean","sd","mean","sd","p.adj")
data <- rbind(
  data.frame(data[,c(1:4)], group=rep("FKB", 41)),
  data.frame(data[c(1,2,5,6)], group=rep("FLB", 41)),
  data.frame(data[c(1,2,7,8)], group=rep("FRB", 41)),
  data.frame(data[c(1,2,9,10)], group=rep("FSB", 41))
)

# 绘图部分（已移除误差条）
ggplot(data=data,
       aes(x=mean, y=Class2, fill=group)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,20)) +
  geom_bar(position=position_dodge(0.8),
           width=0.8,
           stat="identity") +  # 仅保留柱状图
  facet_grid(Class1 ~ ., scales='free_y', space="free_y") +
  scale_fill_manual(values=c("#8DD2C5", "#BFBCDA", "#F47F72", "#7FB2D5")) +
  theme(
    panel.background=element_rect(fill="white", color="black"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_rect(fill=NA, color="black"),
    strip.background=element_rect(fill="grey"),
    strip.text.y=element_text(size=14, face="bold", angle=360),
    legend.position="top",
    legend.title=element_text(face="bold", size=12, color="black"),
    legend.text=element_text(face="bold", size=12, color="black"),
    axis.title=element_text(face="bold", size=14, colour="black"),
    axis.ticks.y=element_blank(),
    axis.line.x=element_blank(),
    axis.text=element_text(face="bold", size=12, color="black")
  ) +
  labs(x="Relative abundance/%", y="KEGG Class II") +
  theme(strip.text.y=element_text(size=14, face="bold", angle=360))

# 导出文件
ggsave("KEGG_plot.pdf", width=10, height=13.5, device=cairo_pdf)
ggsave("KEGG_plot.png", width=10, height=13.5, dpi=300, units="in")
ggsave("KEGG_plot.jpg", width=10, height=13.5, dpi=300, units="in")

