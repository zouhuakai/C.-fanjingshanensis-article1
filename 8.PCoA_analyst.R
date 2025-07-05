#首先没有装的包装一下，不会的看下面这个视频：
#https://www.bilibili.com/video/BV1zb411f7UY/?spm_id_from=333.999.0.0&vd_source=6c608e0ed5fffc668a8be4be19756c13

#工作目录自己也记得设置一下，或者就把R语言关掉后，重新从我们这个代码打开R语言，工作目录就会自动设置到你代码的文件夹了

#通过网盘分享的文件：Adobe Illustrator CC.zip window版本
#链接: https://pan.baidu.com/s/1CU48484wAyfYb_3jMkWwzg?pwd=kzj5 提取码: kzj5 

library(vegan)
library(ggplot2)
library(patchwork)

otu_table <- read.delim('otu_table.txt',row.names = 1)
otu_group <- read.delim('otu_group.txt')

#使用brayj距离进行PCoA分析并提取前两轴数据
plot_data <- data.frame(cmdscale(vegdist(t(otu_table), method = 'bray'), k = (nrow(t(otu_table)) - 1), eig = TRUE)$points[,1:2]) 

# 提取列名，便于后面操作。
plot_data$Site <- rownames(plot_data)

#为样本点坐标添加分组信息
plot_data <- merge(plot_data, otu_group, by = 'Site', all.x = TRUE)
write.csv(plot_data,"PCoA.结果.csv")
#前 2 轴解释量
pcoa1 <- paste('PCoA1 :', round((100*cmdscale(vegdist(t(otu_table), method = 'bray'), k = (nrow(t(otu_table)) - 1), eig = TRUE)$eig/sum(cmdscale(vegdist(t(otu_table), method = 'bray'), k = (nrow(t(otu_table)) - 1), eig = TRUE)$eig))[1], 2), '%')
pcoa2 <- paste('PCoA2 :', round((100*cmdscale(vegdist(t(otu_table), method = 'bray'), k = (nrow(t(otu_table)) - 1), eig = TRUE)$eig/sum(cmdscale(vegdist(t(otu_table), method = 'bray'), k = (nrow(t(otu_table)) - 1), eig = TRUE)$eig))[2], 2), '%')

# 基于bray-curtis距离进行
set.seed(2)
dune.div <- adonis2(t(otu_table) ~ Group, data = otu_group, permutations = 999, method="bray")
dune.div
write.csv(dune.div,"permanova_adonis_结果.csv")

#### 绘制pcoa图 ####
p <- ggplot(plot_data, aes(X1, X2)) +
  geom_point(aes( color=Group), size = 3) +
  stat_ellipse(aes(color = Group),level=0.95)+
  scale_color_manual(values = c('#73bbaf','#d15b64','#592c93','#64A10E4E','#9300264E','#464E044E','#049a0b4E'))+
  labs(x = pcoa1, y = pcoa2)  +
  theme_bw()+
  geom_hline(yintercept = 0, linetype = 'dotted', color = "grey60",linewidth=0.8)+
  geom_vline(xintercept = 0, linetype = 'dotted', color = "grey60",linewidth=0.8)+
  theme(legend.position = c(0.92,0.87),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black",size=10),
        axis.title = element_text(size = 12),
        text = element_text(size = 8))
        
p
ggsave("PCoA.pdf", p ,width = 4.5,height = 4.5)

p1 <- ggplot(plot_data, aes(X1, X2)) +
  geom_point(aes( color=Group), alpha = 0,size = 0) +
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  scale_x_continuous(limits = c(-0.1,0.1))+
  scale_y_continuous(limits = c(-0.1,0.1))+
  labs(x = "", y = "")+
  annotate("text", x = 0, y = 0, label = paste("PERMANOVA\n P =", round(dune.div$`Pr(>F)`[1], 5),"\nR2 =",round(dune.div$R2[1],2)),
           size = 2.5, color = "black")
p1


# Boxplots for PCoA1 and PCoA2 by group
boxplot_pcoa1 <- ggplot(plot_data, aes(x = Group, y = X1, fill = Group)) +
  scale_fill_manual(values = c('#73bbaf','#d15b64','#592c93','#54adf54E','#CDD7E24E','#9295C14E'))+
  geom_boxplot(width = 0.9,outlier.size = 0.8) +
  geom_jitter(width = 0.2, alpha = 1,size = 0.5) +
  coord_flip()+
  theme_bw()+
  labs(x = "", y = "", title = NULL)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank())
boxplot_pcoa1

boxplot_pcoa2 <- ggplot(plot_data, aes(x = Group, y = X2, fill = Group)) +
  scale_fill_manual(values = c('#73bbaf','#d15b64','#592c93','#54adf54E','#CDD7E24E','#9295C14E'))+
  geom_boxplot(width = 0.9,outlier.size = 0.8) +
  geom_jitter(width = 0.2, alpha = 1,size =0.5) +
  theme_bw()+
  labs(x = NULL, y = NULL)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank())
boxplot_pcoa2

layout <- (
  (boxplot_pcoa1 + p1+ plot_layout(widths = c(5, 1.1))) /  # p4 占据上左角，右侧有空白空间
    (p+ boxplot_pcoa2 + plot_layout(widths = c(4, 1)))) +  # p1 在左下角较大，p2 在右下角较小
  plot_layout(heights = c(1, 5)) 
# 显示布局
print(layout)

# 导出组合布局为PDF（新增代码）
ggsave("组合布局.pdf", plot = layout, width = 8, height = 6, device = "pdf")
