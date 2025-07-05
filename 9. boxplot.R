# 加载必要的包
library(tidyverse)
library(rstatix)
library(ggpubr)
library(cowplot)

# 1. 读取数据
alpha_data <- read.csv("M_MW_merge_alpha diversity.csv", header = TRUE) %>%
  mutate(Group = factor(Group, levels = c("M", "W")))  # 确保分组因子顺序

# 检查数据中是否包含M组 (修正位置)
if (!"M" %in% levels(alpha_data$Group)) {
  stop("Error: Moss layer (M) samples not found in data. Please check data file.")
}

# 2. 定义配色方案 - 直接使用红色和蓝色
box_colors <- c("M" = "red", "W" = "blue")

# 3. 创建每个指数的箱线图函数 (修正aes括号错误)
create_boxplot <- function(index_name, y_label) {
  # 统计检验
  stat_test <- alpha_data %>%
    group_by(Group) %>%
    shapiro_test(!!sym(index_name)) %>%
    mutate(normal = ifelse(p > 0.05, TRUE, FALSE))
  
  # 根据正态性选择检验方法
  if (all(stat_test$normal)) {
    var_test <- levene_test(alpha_data, reformulate("Group", index_name))
    if (var_test$p > 0.05) {
      test_method <- "t.test"
    } else {
      test_method <- "wilcox.test"
    }
  } else {
    test_method <- "wilcox.test"
  }
  
  # 执行检验
  if (test_method == "t.test") {
    test_result <- t_test(alpha_data, reformulate("Group", index_name))
    p_val <- test_result$p
  } else {
    test_result <- wilcox_test(alpha_data, reformulate("Group", index_name))
    p_val <- test_result$p
  }
  
  # 转换p值为星号标记
  sig_symbol <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01 ~ "**",
    p_val < 0.05 ~ "*",
    TRUE ~ "ns"
  )
  
  # 计算y轴最大值用于放置显著性标记
  y_max <- max(alpha_data[[index_name]], na.rm = TRUE)
  
  # 绘制箱线图 (修正aes括号)
  ggplot(alpha_data, aes(x = Group, y = !!sym(index_name), fill = Group)) +
    geom_boxplot(aes(fill = Group), outlier.shape = 16, outlier.size = 2) +
    geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) +
    scale_fill_manual(values = box_colors) +
    labs(title = index_name, 
         y = y_label, 
         x = "") +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    # 添加显著性标记
    geom_signif(
      comparisons = list(c("M", "W")),
      annotations = sig_symbol,
      y_position = y_max * 1.1,
      textsize = 6,
      vjust = 0.5
    ) +
    # 添加p值标签
    geom_text(
      x = 1.5, y = y_max * 1.2,
      label = paste0("p = ", format.pval(p_val, digits = 3)),
      size = 3.5
    )
}

# 4. 创建所有指数的箱线图
shannon_plot <- create_boxplot("Shannon", "Shannon Index")
simpson_plot <- create_boxplot("Simpson", "Simpson Index")
chao1_plot <- create_boxplot("Chao1", "Chao1 Index")
ace_plot <- create_boxplot("ACE", "ACE Index")

# 5. 合并所有图形
combined_plot <- plot_grid(
  shannon_plot, simpson_plot, chao1_plot, ace_plot,
  ncol = 2, 
  labels = "AUTO",
  label_size = 14
)

# 6. 添加总标题
title_gg <- ggdraw() + 
  draw_label(
    "Alpha Diversity Comparison between Moss (M) and Water (W) Layers",
    fontface = "bold",
    size = 16,
    x = 0.5,
    hjust = 0.5
  )

final_plot <- plot_grid(
  title_gg,
  combined_plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# 7. 保存图形
# PNG格式
ggsave("alpha_diversity_boxplots.png", final_plot, 
       width = 12, height = 10, dpi = 300)

# PDF格式
ggsave("alpha_diversity_boxplots.pdf", final_plot, 
       width = 12, height = 10, device = cairo_pdf)

# 显示图形
print(final_plot)

