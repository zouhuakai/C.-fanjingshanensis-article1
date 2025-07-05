# 加载必要包
library(VennDiagram)
library(gridExtra)
library(dplyr)

# 设置文件和分组
file_names <- c("FKB.txt", "FLB.txt", "FRB.txt", "FSB.txt")
group_names <- c("FKB", "FLB", "FRB", "FSB")

# 读取数据并转换为向量列表
asv_list <- lapply(file_names, function(x) readLines(x) %>% trimws())
names(asv_list) <- group_names

# 核心修改1：计算四组交集并保存
get_intersections <- function(lst){
  # 计算所有可能的交集组合
  all_comb <- lst %>% 
    gplots::venn(show.plot = FALSE) %>% 
    with(intersections)
  
  # 提取四组共同ASV
  quad_intersect <- Reduce(intersect, lst)
  write.csv(data.frame(ASV=quad_intersect), 
            "Quad_Intersection.csv", row.names = FALSE)
  
  return(all_comb)
}

# 执行交集计算
intersection_data <- get_intersections(asv_list)

# 核心修改2：优化标签位置参数
create_venn <- function(){
  venn.plot <- venn.diagram(
    x = asv_list,
    category.names = group_names,
    filename = NULL,
    output = TRUE,
     
    # 布局优化参数
    rotation.degree = 0,       # 取消默认旋转
    cat.pos = c(0, 0, 0, 0),   # 所有标签位于正上方
    cat.dist = c(0.15, 0.15, 0.15, 0.15),  # 统一标签距离
    cat.fontfamily = "sans",
    
    # 保持原有美学参数
    fill = c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D"),
    alpha = 0.4,
    lwd = 0.5,
    cex = 0.8,
    margin = 0.08
  )
  return(venn.plot)
}

# 输出图形（保持原有输出设置）
pdf("ASV_Venn.pdf", width=7, height=7)
grid.draw(create_venn())
dev.off()

png("ASV_Venn.png", width=7, height=7, units="in", res=3000)
grid.draw(create_venn())
dev.off()
