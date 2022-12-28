###########################消极评价的词云图
com_neg <- subset(com,sen_label=1)
write.csv(com_neg,"D:/人力资源管理课程/3-上-人力资源统计学/报告/neg.csv")
comment_neg <- com_neg$content

library(stringr)
#去掉一些无用高频词语：
comment_neg <- str_replace_all(comment_neg,"人力资源","")
comment_neg <- str_replace_all(comment_neg,"人力","")
comment_neg <- str_replace_all(comment_neg,"管理","")
comment_neg <- str_replace_all(comment_neg,"企业","")
comment_neg <- str_replace_all(comment_neg,"公司","")
comment_neg <- str_replace_all(comment_neg,"工作","")
comment_neg <- str_replace_all(comment_neg,"谢邀","")
comment_neg <- str_replace_all(comment_neg,"HR","")
comment_neg <- str_replace_all(comment_neg,"hr","")
comment_neg <- str_replace_all(comment_neg, "[^[:alnum:]]", "")
comment_neg <- str_replace_all(comment_neg,"1","")
comment_neg <- str_replace_all(comment_neg,"2","")
comment_neg <- str_replace_all(comment_neg,"3","")
comment_neg <- str_replace_all(comment_neg,"4","")
comment_neg <- str_replace_all(comment_neg,"5","")
comment_neg <- str_replace_all(comment_neg,"6","")
comment_neg <- str_replace_all(comment_neg,"7","")
comment_neg <- str_replace_all(comment_neg,"8","")
comment_neg <- str_replace_all(comment_neg,"9","")
comment_neg <- str_replace_all(comment_neg,"0","")

#采用哈工大停词系统进行分词
engine <- worker(type = "mix",stop_word = "D:/Project/stopwords-master/hit_stopwords.txt" )
dic <- readLines("D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/可视化/HR.txt")##增加专业词汇词典，来自搜狗词库
new_user_word(engine, dic)
seg_neg <- segment(comment_neg, engine)

wordfreqs <- freq(seg_neg)
wordfreqs <- dplyr::arrange(wordfreqs, -freq)

#删除一些影响结果的傻逼介词、感叹词等
del_chara <-  readLines("D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/可视化/删除的词.txt")
del <- which(wordfreqs$char %in% del_chara)
wordfreqs <- wordfreqs[-del,]
#词频结果
head(wordfreqs,20)
write.table(wordfreqs,"D:/人力资源管理课程/3-上-人力资源统计学/报告/DataAnalysis/可视化/结果-Neg.txt")
#选择词频大于4的词语进行可视化
del_freq <- which(wordfreqs$freq>8)
wordfreqs <- wordfreqs[del_freq,]
wordcloud2(wordfreqs,size =0.5, fontFamily = "微软雅黑",  color = "Blue",shape = "cloud")