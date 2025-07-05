#加载需要的包
#install.packages("plotrix")
library(plotrix)

#导入数据
flower_dat <- read.csv('result.csv')
flower_dat[is.na(flower_dat)] <- ''#将NA转为空格

#运行自建函数
flower <- function(flower_dat, start, a, b, r, ellipse_col, circle_col) {
  sample <- colnames(flower_dat)
  otu_id <- unique(flower_dat[,1])
  otu_id <- otu_id[otu_id != '']
  core_otu_id <- otu_id
  otu_num <- length(otu_id)
  
  for (i in 2:ncol(flower_dat)) {
    otu_id <- unique(flower_dat[,i])
    otu_id <- otu_id[otu_id != '']
    core_otu_id <- intersect(core_otu_id, otu_id)
    otu_num <- c(otu_num, length(otu_id))
  }
  core_otu <- length(core_otu_id)
  write.csv(core_otu_id, "交集.csv",row.names = F)
  par( bty = 'n', ann = F, xaxt = 'n', yaxt = 'n', mar = c(1,1,1,1))
  plot(c(0,10),c(0,10),type='n')
  n   <- length(sample)
  deg <- 360 / n
  res <- lapply(1:n, function(t){
    draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                 y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                 col = ellipse_col[t],
                 border = ellipse_col[t],
                 a = a, b = b, angle = deg * (t - 1))
    text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
         y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
         otu_num[t])
    
    if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) - start,
           adj = 1,
           cex = 1
      )
    } else {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) + start,
           adj = 0,
           cex = 1
      )
    }			
  })
  draw.circle(x = 5, y = 5, r = r, col = circle_col, border = NA)
  text(x = 5, y = 5, paste( core_otu))
}

# 输出PDF格式
pdf('花瓣图1.pdf', width = 5, height = 5, family = "serif") # 新罗马字体
flower(flower_dat,
       start = 90, 
       a = .7, # 花瓣椭圆的宽度
       b = 2,  # 花瓣椭圆位于第几个轨道
       r = 1,  # 中心圆半径
       ellipse_col = c('#6181BD4E','#F348004E','#64A10E4E','#9300264E','#464E044E','#049a0b4E','#4E0C664E','#D000004E','#FF6C004E','#FF00FF4E','#c7475b4E','#00F5FF4E','#BDA5004E','#A5CFED4E','#f0301c4E','#2B8BC34E','#FDA1004E','#54adf54E','#CDD7E24E','#9295C14E'),
       circle_col = 'white')
dev.off()

# 新增高清PNG输出（4000 DPI）
png('花瓣图1.png', width = 5, height = 5, units = "in", res = 4000, family = "serif")
flower(flower_dat,
       start = 90, 
       a = .7, 
       b = 2,  
       r = 1,  
       ellipse_col = c('#6181BD4E','#F348004E','#64A10E4E','#9300264E','#464E044E','#049a0b4E','#4E0C664E','#D000004E','#FF6C004E','#FF00FF4E','#c7475b4E','#00F5FF4E','#BDA5004E','#A5CFED4E','#f0301c4E','#2B8BC34E','#FDA1004E','#54adf54E','#CDD7E24E','#9295C14E'),
       circle_col = 'white')
dev.off()
