#Phan tich du lieu 


install.packages("dplyr")
install.packages("tidyverse")

library(tidyverse)
library(dplyr)
library(ggplot2)


setwd("C:/Users/84327/Data_Covid/data")
df <- read.csv("25-12-2021.csv",encoding = "UTF-8" , header = T, stringsAsFactors = F)


# I. lam sach du lieu
df_1 <- df


df_1$ToTal <- as.numeric(df_1$ToTal)
df_1$Daynow <- as.numeric(df_1$Daynow)
df_1$Die <- as.numeric(df_1$Die)

str(df_1)

df_1[is.na(df_1)] = 0
# II. Phan tich thong ke mo ta so lieu
# tinh toan cac chi so thong ke mo ta
mean(df_1$Daynow) # trung binh
mode(df_1$Daynow) # yeu vi
median(df_1$Daynow) # trung vi
var(df_1$Daynow) # phuong sai
sd(df_1$Daynow) # do lech chuan  
min(df_1$Daynow) # gia tri nho nhat  
max(df_1$Daynow) # gia tri lon nhat  

# II.1/ ve bieu do truc quan hoa du lieu
#a . histogram
ggplot(df_1, aes(x=City,y=Daynow))+
  geom_col(aes(fill=City))+
  labs(title='CoVid-19 NGÀY 29-04-2021',
       x='Khu vuc',
       y='so ca nhiem')





