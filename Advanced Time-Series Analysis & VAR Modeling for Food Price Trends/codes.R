setwd('C:/Users/Adebolu/Documents/R/Project')
library(readxl)
library(ggplot2)
library(reshape2)

library(dplyr)
library(lubridate)
library(janitor)
library(psych)
library("stringr") 
library("Hmisc")
library(corrplot)
library(tseries)
library("forecast")
library(tidyr)
library(zoo)
library(DescTools)
library(e1071)
library(urca)
library(ggcorrplot)

library(seasonal)
library(vars)
library(clipr)
food<- read_excel("Data for Project.xlsx")
output <- capture.output( {
  print(n=71, food)
})
write_clip(output)
food <- clean_names(food)
food$date <- as.Date(food$date,"%d/%m/%Y") 
Number of NAs


newmillet <- na.approx(food$millet, na.rm = FALSE) 
food$millet <- newmillet

newirice <- na.approx(food$imported_rice_50kg, na.rm = FALSE) 
food$imported_rice_50kg <- newirice

newlrice <- na.approx(food$local_rice_50kg, na.rm = FALSE) 
food$local_rice_50kg <- newlrice

newrbeans <- na.approx(food$red_beans, na.rm = FALSE) 
food$red_beans <- newrbeans

newwbeans <- na.approx(food$white_beans_50kg, na.rm = FALSE) 
food$white_beans_50kg <- newwbeans

newegg <- na.approx(food$eggs_1_crate, na.rm = FALSE) 
food$eggs_1_crate <- newegg

newgarri <- na.approx(food$garri_50kg, na.rm = FALSE) 
food$garri_50kg <- newgarri

newoil <- na.approx(food$oil_1l, na.rm = FALSE) 
food$oil_1l <- newoil

newbeef <- na.approx(food$beef_kg, na.rm = FALSE) 
food$beef_kg <- newbeef

View(food)

food<- food%>%na.omit()
gfood <-food[, -1]



p <- ggplot(melt(gfood), aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of All") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the ggplot object
print(p)

summary(food)
stddev <- sapply(food, sd)
stddev

data1 <- select(food, -date)
CV <- apply(data1,2,function(x) (sd(x)/mean(x))*100)

str(food)

change <- food%>%select( -date)%>%food[nrow(food),]-food[1,]
Divide the change by the first value of each column and multiply by 100:
  Copy code
percentage_change <- (change/food[1,])*100

Percent change 

Yearly_average <- food %>%
  group_by(year(date)) %>%
  summarise(across(where(is.numeric), mean))


for (col in names(Yearly_average)[-1]) {
  symbol <- str_sub(col, 1, -3)
  new_col_name <- paste(symbol, "% Change")
  col_values <- Yearly_average[[col]]
  Yearly_average[[new_col_name]] <- 100*(col_values - lag(col_values))/lag(col_values)
}
View(Yearly_average)
Yearly_average<-Yearly_average[-1,]
colnames(Yearly_average)
Yearly_average <- data.frame(Yearly_average)

Yearly_average <- Yearly_average %>% dplyr::select(!(millet:beef_kg))

names(Yearly_average)[-1] <- str_sub(names(Yearly_average)[-1], 1, -10)
Yearly_average <- gather(Yearly_average, key = "Symbol", value = "% Change", mill:beef_)


Yearly_average %>% 
  group_by(Symbol) %>% 
  summarise(`Standard Deviation of % Change` = sd(`% Change`)) %>% 
  arrange(desc(`Standard Deviation of % Change`))

sd(food)
sapply(food[,2:10], sd)

PERCENTAGE CHANGE 


millettrend<- food%>% ggplot(aes(x=date, y=millet)) +
  geom_line(size = 1.2, color="#66CDAA") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels="%Y",date_breaks ="1 year")+
  ggtitle('Price of Millet')+
  labs(x="Years",y="Price")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
ggsave("mymillet.png", plot = millettrend)

 rice1<-food%>% ggplot(aes(x=date, y= imported_rice_50kg)) +
  geom_line(size = 1.2, color="#66CDAA") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels="%Y",date_breaks ="1 year")+
  ggtitle('Price of imported rice per 50kg')+
  labs(x="Years",y="Price")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
 ggsave("myrice1.png", plot = rice1)
 
 
 rice2<- food%>% ggplot(aes(x=date, y= local_rice_50kg)) +
   geom_line(size = 1.2, color="#66CDAA") + 
   theme_bw()+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
   scale_x_date(date_labels="%Y",date_breaks ="1 year")+
   ggtitle('Price of local_rice_50kg')+
   labs(x="Years",y="Price")+
   theme(plot.title = element_text(hjust = 0.5, face="bold"))
 ggsave("myrice2.png", plot = rice2)
 
 beans1<-food%>% ggplot(aes(x=date, y= red_beans)) +
  geom_line(size = 1.2, color="#66CDAA") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels="%Y",date_breaks ="1 year")+
  ggtitle('Price of red beans')+
  labs(x="Years",y="Price")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
 ggsave("mybeans1.png", plot = beans1)
 
 beans2<-food%>% ggplot(aes(x=date, y= white_beans_50kg)) +
  geom_line(size = 1.2, color="#66CDAA") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels="%Y",date_breaks ="1 year")+
  ggtitle('Price of white beans per 50kg')+
  labs(x="Years",y="Price")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
 ggsave("mybeans2.png", plot = beans2)
 
 eggs1<-food%>% ggplot(aes(x=date, y= eggs_1_crate)) +
  geom_line(size = 1.2, color="#66CDAA") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels="%Y",date_breaks ="1 year")+
  ggtitle('Price of egg per crate')+
  labs(x="Years",y="Price")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
 ggsave("myeggs.png", plot = eggs1)
 
 garri1<-food%>%ggplot(aes(x=date, y= garri_50kg)) +
  geom_line(size = 1.2, color="#66CDAA") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels="%Y",date_breaks ="1 year")+
  ggtitle('Price of garri per 50kg')+
  labs(x="Years",y="Price")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
 ggsave("mygarri.png", plot = garri1)
 
 oil1<-food%>% ggplot(aes(x=date, y= oil_1l)) +
  geom_line(size = 1.2, color="#66CDAA") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels="%Y",date_breaks ="1 year")+
  ggtitle('Price of oil per litre')+
  labs(x="Years",y="Price")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
 ggsave("myoil.png", plot = oil1)
 
 beef1<- food%>% ggplot(aes(x=date, y= beef_kg)) +
  geom_line(size = 1.2, color="#66CDAA") + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels="%Y",date_breaks ="1 year")+
  ggtitle('Price of beef per kg')+
  labs(x="Years",y="Price")+
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
 ggsave("mybeef.png", plot = beef1) 
 
 
foodts<- food[,-1]
food.ts = ts(food,
           frequency = 12,
           start = c(2016, 7))
plotss<- plot.ts(food.ts)



apply(food.ts, 2, adf.test)

ndiffs(food.ts)

sfood.ts<- diff(food.ts,differences = 1)
apply(sfood.ts, 2, adf.test)
plot(sfood.ts)

sfood.ts2<-diff(sfood.ts, differences = 1)
apply(sfood.ts2, 2, adf.test)
plot(sfood.ts2)


p_values <- rcorr(as.matrix(sfood.ts2))



ggcorrplot(p_values$r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

VARselect(sfood.ts2, lag=20, type = "const")
vm1 = VAR(sfood.ts2,ic = "AIC", p=5, type="none")
causality(vm1, #VAR model
          cause = c("red_beans")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 



coefficients(vm)

NAMES = colnames(sfood.ts)
k = ncol(sfood.ts)
 for (j in 1:k) {
  for (i in 1:k) {
    if (i != j) {
      print(paste(NAMES[j], "->", NAMES[i]))
      VARest = vars::VAR(sfood.ts[,c(j,i)], p=1)
      print(causality(VARest, cause=NAMES[j]))
    }
  }
}

output <- capture.output( {
  k = ncol(sfood.ts)
  for (j in 1:k) {
    for (i in 1:k) {
      if (i != j) {
        print(paste(NAMES[j], "->", NAMES[i]))
        VARest = vars::VAR(sfood.ts[,c(j,i)], p=1)
        print(causality(VARest, cause=NAMES[j]))
      }
    }
  }
})
write_clip(output)
