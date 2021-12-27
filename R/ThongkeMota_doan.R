#Phan tich du lieu 


install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggrepel")

library(ggrepel)
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
#b .pie chart
die = c(df_1$Die[1:5],sum(df_1$Die[6:63]))
die

haha = data.frame(city = c(df_1$City[1:5],'Còn lại') , die = die)
haha

sum(haha$die)
haha$phan_tram = (format(round((haha$die / sum(haha$die)), 2), nsmall = 2))
haha$phan_tram <- as.numeric(haha$phan_tram)*100
haha
huhu = haha %>% 
  mutate(csum = rev(cumsum(rev(phan_tram))), 
         pos = phan_tram/2 + lead(csum, 1),
         pos = if_else(is.na(pos), phan_tram/2, pos))
huhu

ggplot(huhu, aes(x = "", y = die, fill = city)) +
  geom_col(color = "black") +
  geom_text(aes(label = phan_tram),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#ebf2ff"),
        plot.background = element_rect(fill = "#ebf2ff"),
        legend.background = element_rect(fill = "#ebf2ff"))

