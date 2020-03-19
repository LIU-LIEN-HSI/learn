library(tidyverse)
library(data.table)              #用來讀取大型資料配合函數"fread"
library(ggplot2)
library(dplyr)

setwd("C:/Users/LIU/Desktop/analy")   #設定路徑資料夾

df <- fread('w_outstanding.csv') #fread("檔案名稱")
df <- df %>%
    filter(allv>0) %>%                             #首先先篩選出allv(發行張數)>0的數據
    mutate(outv=ifelse(outv>allv, allv, outv)) %>% #新增欄位，名稱:outv，條件(若賣出張數>發行張數，取發行張數，否，則取賣出張數)
    mutate(outr=outv/allv) %>%                     #新增欄位，比例=outv/allv
    mutate(leftv=allv-outv) %>%                    #新增欄位，保留張數=allv-outv
    group_by(wid, wn) %>%                          #將資料依(wid)分群，(wn)分群
    summarise(max_outr=max(outr),                  #取出最大的賣出比例
              min_leftv=min(leftv)) %>%            #取出最小的保留張數
    ungroup() %>%                                  #去除已分組的數據
    mutate(iss=ifelse(nchar(wn)==10, substr(wn,4,5), substr(wn,3,4))) #新增欄位，名稱:iss，條件(若wn字數=10，取wn的第4~5字，否，則取wn的第3~4字)
                                                                      #nchar()用來統計向量的長度
                                                                      #substr(字串名稱,從第X字開始,第X字結束)
iss_df <- df %>%
    group_by(iss) %>%                                                 #將資料依(iss)分群
    summarise(avg_maxoutr=mean(max_outr),                             #賣出比例最大的平均值
              q80_maxoutr=quantile(max_outr, 0.8),                    #賣出比例最大的第80%分位數
              avg_minleftv=mean(min_leftv),                           #保留張數最小的平均值
              q20_minleftv=quantile(min_leftv, 0.2)) %>%              #保留張數最小的第20%分位數
    ungroup()


ggplot(df) +                                                         #ggplot(data名稱)，以+號連結不同圖層
    geom_histogram(aes(max_outr)) +                                  #畫直方圖_aes(x軸=名稱,y軸=名稱)
    facet_wrap(.~iss, scales='free') +                               #facet_wrap將圖纏繞分面(一頁多圖)，並依iss進行數據分類，座標刻度:依各圖數據範圍自由調整
    geom_vline(data=iss_df, aes(xintercept=avg_maxoutr, colour="mean")) +  #垂直線，X軸=賣出最大比例的平均值
    geom_vline(data=iss_df, aes(xintercept=q80_maxoutr, colour="80%")) +   #垂直線，X軸=賣出最大比例的第80%分位數
    theme_bw() +                                                           #ggplot主題模板
    labs(title='各券商單檔權證最大在外流通比例分布')
ggsave('各券商單檔權證最大在外流通比例分布.png', width=40, height=20, units='cm') #保存繪圖形式


ggplot(df) +
    geom_histogram(aes(min_leftv)) +
    scale_x_continuous(breaks = seq(0, 2000, 500), limits=c(0, 2000)) +     #控制X軸的形式，breaks:控制坐標軸上應該顯示哪些刻度線的值，limits:固定的範圍
    facet_wrap(.~iss, scales='free') +
    geom_vline(data=iss_df, aes(xintercept=avg_minleftv, colour="mean")) +
    geom_vline(data=iss_df, aes(xintercept=q20_minleftv, colour="20%")) + 
    theme_bw() + 
    labs(title='各券商單檔權證最小剩餘張數分布')
ggsave('各券商單檔權證最小剩餘張數分布.png', width=40, height=20, units='cm')
