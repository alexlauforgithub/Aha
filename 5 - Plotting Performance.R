# 計算策略績效
WinRateCount <- function(x){length(which(x>0))/length(x)}

# 繪製多圖函數
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# 統計各月份交易發生次數
tradeMonthNums <- data %>%
  mutate(tradeMonth=substr(DailyDate,1,6)) %>%
  group_by(tradeMonth) %>%
  summarise(tradeNums=n(),
            avgRet=mean(LeadRetPercentage),
            winRate=WinRateCount(LeadRetPercentage))

# 繪製各月份交易次數長條圖
p1 <- ggplot(data=tradeMonthNums, aes(x=tradeMonth, y=tradeNums)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=tradeNums), vjust=-0.3, size=3.5)+
  theme_minimal()+
  #labs(x="月份", y ="交易次數")+
  theme(axis.text.x=element_text(angle=90)) +
  theme(text = element_text(size=15))

# 繪製各月份平均報酬率(含交易成本)長條圖
p2 <- ggplot(data=tradeMonthNums, aes(x=tradeMonth, y=avgRet)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=paste(round(avgRet*100,2),"%")), vjust=-0.3, size=3.5)+
  theme_minimal()+
  #labs(x="月份", y ="平均報酬率(含交易成本)")+
  theme(axis.text.x=element_text(angle=90)) +
  theme(text = element_text(size=15))

# 繪製各月勝率長條圖
p3 <- ggplot(data=tradeMonthNums, aes(x=tradeMonth, y=winRate)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=paste(round(winRate*100,2),"%")), vjust=-0.3, size=3.5)+
  theme_minimal()+
  #labs(x="年份", y = "勝率")+
  theme(axis.text.x=element_text(angle=90)) +
  theme(text = element_text(size=15))

multiplot(p1, p2, p3, cols=1)