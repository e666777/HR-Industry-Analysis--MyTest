library(jiebaR)
library(wordcloud2)
library(dplyr)
library(stringr)
########词典法进行情感分析########
com<-read.csv("D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/SentimentAnalysis/output/result.csv",header=T) 
comment<-com$content

library(stringr)
#去掉一些无用高频词语：
comment <- str_replace_all(comment,"人力资源","")
comment <- str_replace_all(comment,"人力","")
comment <- str_replace_all(comment,"管理","")
comment <- str_replace_all(comment,"企业","")
comment <- str_replace_all(comment,"公司","")
comment <- str_replace_all(comment,"工作","")
comment <- str_replace_all(comment,"谢邀","")
comment <- str_replace_all(comment,"HR","")
comment <- str_replace_all(comment,"hr","")
comment <- str_replace_all(comment, "[^[:alnum:]]", "")
comment <- str_replace_all(comment,"1","")
comment <- str_replace_all(comment,"2","")
comment <- str_replace_all(comment,"3","")
comment <- str_replace_all(comment,"4","")
comment <- str_replace_all(comment,"5","")
comment <- str_replace_all(comment,"6","")
comment <- str_replace_all(comment,"7","")
comment <- str_replace_all(comment,"8","")
comment <- str_replace_all(comment,"9","")
comment <- str_replace_all(comment,"0","")

#采用哈工大停词系统进行分词
engine <- worker(type = "mix",stop_word = "D:/Project/stopwords-master/hit_stopwords.txt" )
dic <- readLines("D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/可视化/HR.txt")##增加专业词汇词典，来自搜狗词库
new_user_word(engine, dic)
seg <- segment(comment, engine)

wordfreqs <- freq(seg)
wordfreqs <- dplyr::arrange(wordfreqs, -freq)

#删除一些影响结果的傻逼介词、感叹词等
del_chara <-  readLines("D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/可视化/删除的词.txt")
del <- which(wordfreqs$char %in% del_chara)
wordfreqs <- wordfreqs[-del,]
#词频结果
head(wordfreqs,20)
write.table(wordfreqs,"D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/可视化/结果.txt")
#选择词频大于8的词语进行可视化
del_freq <- which(wordfreqs$freq>8)
wordfreqs <- wordfreqs[del_freq,]
wordcloud2(wordfreqs,size =0.5, fontFamily = "微软雅黑",  color = "random-light",shape = "cloud")





###########################积极评价的词云图
com_pos <- subset(com,sen_label=1)
write.csv(com_pos,"D:/人力资源管理课程/3-上-人力资源统计学/报告/pos.csv")
#原来没有正面评价啊,我还以为我搞错了。符合我的预期，人力资源狗都不读

