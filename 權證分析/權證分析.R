library(tidyverse)
library(data.table) #用來讀取大型資料配合函數"fread"
library(ggplot2)
library(readxl)
library(dbplyr)
library(glue)

setwd("C:/Users/LIU/Desktop/analy/trade_record/") #設定路徑資料夾

#迴圈:一次讀取多個檔案

dlist1 <- c('20200218', '20200219','20200220','20200221','20200224','20200225','20200226','20200227','20200304','20200305','20200306')
wdat_rbind <- NULL
for(d1 in dlist1){                                  #迴圈變數:d1，d1在dlist1的
  tdf1 <- fread(glue('{d1}.csv'))                   #利用glue，將日期黏到 .csv前面
  wdat_rbind <- rbind(wdat_rbind, tdf1)             #透過row合併df和tdf
}


wdat_rbind <- wdat_rbind %>%
  select(wid='代號',date='日期',wn='名稱',strike='最新履約價',exr='最新執行比例') %>%
  mutate(wid=as.character(wid)) %>%
  mutate(date=as.character(date))%>%
  mutate(wid=ifelse(nchar(wid)==5, paste0(0,wid), wid)) %>%
  filter(nchar(wid)==6 )

#迴圈:一次讀取多個檔案

dlist <- c('20200218', '20200219','20200220','20200221','20200224','20200225','20200226','20200227','20200304','20200305','20200306')
df <- NULL
for(d in dlist){                                  #迴圈變數:d，d在dlist的
    tdf <- read_excel(glue('{d}_明細.xlsx')) %>%  #利用glue將日期黏到 _明細.xlsx前面
        mutate(date=d)                            #新增欄位，date=d
    df <- rbind(df, tdf)                          #透過row合併df和tdf
}


df <- df %>%
  select(wid='代號', bs='買賣別', date, volume='成交數量') %>%
  mutate(wid=as.character(wid)) %>%
  mutate(bs=as.character(bs)) %>%
  mutate(wid=ifelse(nchar(wid)==5, paste0(0,wid), wid)) %>%
  filter(nchar(wid)==6 & bs=='賣出') %>%
  left_join(wdat_rbind, by=c('wid', 'date')) %>%
  mutate(iss=ifelse(nchar(wn)==10, substr(wn,4,5), substr(wn,3,4)))

ana <- df %>%
  group_by(wid, iss) %>%
  summarise(count=n(),
            avg_v=mean(volume)) %>%
  ungroup()


  ggplot(ana) +                                  #ggplot(data名稱)，以+號連結不同圖層
  geom_histogram(aes(count)) +                   #畫直方圖_aes(x軸=名稱,y軸=名稱)
  facet_wrap(.~iss, scales='free_y')+            #facet_wrap將圖纏繞分面(一頁多圖)，並依iss進行數據分類
  theme_bw() +                                   #ggplot主題模板
  labs(x='某檔權證交易次數',title='各券商權證的交易次數')
  ggsave('各券商權證的交易次數.png', width=40, height=20, units='cm') #保存繪圖形式

  ggplot(ana) +                                  #ggplot(data名稱)，以+號連結不同圖層
    geom_histogram(aes(avg_v), binwidth=10000) + #畫直方圖_aes(x軸=名稱,y軸=名稱)
    facet_wrap(.~iss, scales='free_y')+          #facet_wrap將圖纏繞分面(一頁多圖)，並依iss進行數據分類
    theme_bw() +                                 #ggplot主題模板
    labs(x='平均成交量(千)',title='各券商的平均成交量')
  ggsave('各券商的平均成交量.png', width=40, height=20, units='cm') #保存繪圖形式
  
  
    