# 加载所需包
library(ggplot2)
library(ggpubr)
library(tidyr)
library(here)

# 读取数据
data <- read.csv(here("step3.diversity.index.csv"))

# 转换数据为长格式
data_long <- pivot_longer(data, 
                          cols = c(Simpson, Shannon, Chao1, ACE),    #修改指标，一个一个输出
                          names_to = "Alpha_Index", 
                          values_to = "Value")

# 获取实验组别信息
groups <- unique(data$Group)
comparisons <- if(length(groups) > 2) {
  combn(groups, 2, simplify = FALSE)
} else {
  list(groups)
}

# 创建可视化图形
p <- ggplot(data_long, aes(x = Group, y = Value, fill = Group)) +
  geom_violin(trim = FALSE, scale = "width") +
  geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1.5) +
  facet_wrap(~ Alpha_Index, scales = "free_y", ncol = 2) +
  # 添加组间比较（自动适应两组或多组）
  stat_compare_means(
    comparisons = comparisons,
    method = "wilcox.test",
    label = "p.signif",
    p.adjust.method = "BH",     # 使用Benjamini-Hochberg校正
    hide.ns = TRUE,             # 隐藏不显著的结果
    label.y = max(data_long$Value) * 1.1,  # 调整标签位置
    step.increase = 0.08        # 多组比较时的标签间距
  ) +
  labs(x = "Experimental Group", y = "Alpha Diversity Index") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 创建输出目录
if (!dir.exists("output")) dir.create("output")

# 导出图片
ggsave(here("output/alpha_diversity.png"), p,
       width = 10, height = 8, dpi = 600, bg = "white")

ggsave(here("output/alpha_diversity.pdf"), p,
       width = 10, height = 8, device = cairo_pdf, bg = "white")

message("可视化结果已保存至output目录")

