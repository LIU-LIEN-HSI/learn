library(data.table)
library(magrittr)
library(tidyverse)
library(zoo)
library(lubridate)
library(ggplot2)


setwd("C:/Users/LIU/Desktop/analy/")

ivin <- fread("C:/Users/LIU/Desktop/analy/iv_in.csv")
ivout <- fread("C:/Users/LIU/Desktop/analy/iv_out.csv")

ana <- ivin %>%
  inner_join(ivout, by='wid') %>%
  mutate(iv_diff = iv-ivin) %>%
  mutate(s_change = sbid1/avg_sask1-1)

plot(ana$s_change,ana$iv_diff,main='權證進出場時的IV變化對應股價變化')





