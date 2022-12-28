DATA <- read.csv("D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/描述/result-utf8编码.csv")
library()
DATA$字数 <- nchar(DATA$content)


DATA$X <- with(DATA, reorder(X,comment_count))
library(ggplot2)
P1 <- ggplot(DATA, aes(x=X, y=comment_count)) +
  geom_segment( aes(x=X, xend=X, y=0, yend=comment_count), color="#eb9187") +
  geom_point( color="#d11b1b", size=2, alpha=0.3) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  xlab("回答") +
  ylab("评论数")
P1
ggsave(P1,filename = "D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/描述/评论数.jpg",width = 12,height = 9)


DATA_delNA<-subset(DATA,voteup_count!="NA")
DATA_delNA$X <- with(DATA_delNA, reorder(X,voteup_count))
library(ggplot2)
P2 <- ggplot(DATA_delNA, aes(x=X, y=voteup_count)) +
  geom_segment( aes(x=X, xend=X, y=0, yend=voteup_count), color="#87b2eb") +
  geom_point( color="#1b2dd1", size=2, alpha=0.3) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  xlab("回答") +
  ylab("点赞数")
P2
ggsave(P2,filename = "D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/描述/点赞数.jpg",width = 12,height = 9)



DATA$X = with(DATA, reorder(X,字数))
P3 <- ggplot(DATA, aes(x=X, y=字数)) +
  geom_segment( aes(x=X, xend=X, y=0, yend=字数), color="#eb87dc") +
  geom_point( color="#b61bd1", size=2, alpha=0.3) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  xlab("回答") +
  ylab("字数")
P3
ggsave(P3,filename = "D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/描述/字数.jpg",width = 12,height = 9)


library(patchwork)
P_All <- (P1|P2)/P3
ggsave(P_All,filename = "D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/描述/All.jpg",width = 12,height = 9)


library(stargazer)
library(Hmisc)
stargazer(DATA, type = "text",digits = 2)


prop.table(table(DATA$sen_label))


DATA$文本性质[which(DATA$sen_label =='-1')] <- "负向情感"
DATA$文本性质[which(DATA$sen_label =='0')] <- "正向情感"
#分组柱状图
P4 <- ggplot(DATA, aes(x=X, y=sen_scores, fill=sen_label)) +
    geom_bar(stat="identity", position="identity")
ggsave(P4,filename = "D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/描述/得分.jpg",width = 12,height = 9)
