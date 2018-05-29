
#Name :-Smruti Venkatesh Ubitno:-50247212
#Name :-Prachi Jayakumar Patil Ubitno:-50247309

library(ggplot2)

influenza1 = read.table("./data/influenza.txt",header=T)
theme_set(theme_classic())
week = paste(c(influenza1$Week))
total_a = c(influenza1$Total_A)
total_b = c(influenza1$Total_B)
total = c(total_a + total_b)
percent_postive_a = c(influenza1$Percent_Positive_A)
percentt_postive_b = c(influenza1$Percent_Positive_B)

x=1:10
y=11:20

data=t(matrix(c(influenza1$Total_B, influenza1$Total_A), nrow=17))

barplot(data, col=c( "Pink", "Blue"), names.arg=paste(influenza1$Week))

#plot(x = input$Week, y = input$Percent.Positive.A, type="o", col = "yellow")
#lines(input$Percent.Positive.A) 
par(new = TRUE)
plot(x = c(1:17),y = influenza1$Percent_Positive_A, ylim = c(0,27), col = "yellow", type="l", lty=2, ylab='', xlab = '', axes = FALSE)
par(new = TRUE)
plot(x = c(1:17), y = influenza1$Percent_Positive_B, ylim = c(0,27), col = "green", type="l", lty=2, ylab='', xlab = '', axes = FALSE)
par(new = TRUE)
plot(x = c(1:17), y = influenza1$Percent_Positive, ylim = c(0,27), col = "black", type="l", ylab='', xlab = '', axes = FALSE)
#par(new = TRUE)
#plot(x = c(1:17), y = input$Percent.Positive.B, ylim = c(0,25), type="o", col = "green")

library(dplyr)
library(reshape2)
library(scales)
library(ggplot2)

influenza2<-read.table("./data/influenza_2.txt", header=T)
#influenza2
df<- influenza2[,c(1,2,3,4,6,7,8,9)]
mdfr <- melt(df, id.var = "Week")

ggplot(mdfr, aes(x=as.character(Week), y=value, fill=variable)) + 
  geom_bar(stat="identity") +
  xlab("\nWeek") +
  ylab("Total_tested\n") +
  theme_bw()




data<-read.table(file.choose(), fill=TRUE, header=T)
xdata<- data$MMWR-WEEK
y1<- data$WEEKLY RATE 
y2<- data$Percent_Positive_A
y3<- data$Percent_Positive_B
plot(xdata, y1, type="l", col="black", pch="l",lty=1)



#df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
 #                  variable=rep(paste0("category", 1:9), each=5))
influenza1<-read.csv(file.choose(), fill=TRUE, header=T)
df<- influenza1[,c(4,5,6,8)]
df<- as.data.frame(df)
ggplot(data = df, aes(x=MMWR.WEEK, y=WEEKLY.RATE)) + geom_line(aes(colour=variable))

library("reshape2")

influenzaHosp<-read.csv(file.choose(), fill=TRUE, header=T, as.is=TRUE, na.strings=c(NA, "NA", " NA"))
df<- influenzaHosp[complete.cases(influenzaHosp[,c(4,5,6,7)]), ]
mdf<- as.data.frame(df)

df$YearWeek=paste(df$MMWR.YEAR,df$MMWR.WEEK,sep="_")
df0=df%>%select(YearWeek,CUMULATIVE.RATE,AGE.CATEGORY)%>%melt(id.vars="YearWeek")
ggplot(data=df, aes(x=YearWeek, y=CUMULATIVE.RATE, group=AGE.CATEGORY)) +
  geom_line(aes(color=AGE.CATEGORY))+
  geom_point(aes(color=AGE.CATEGORY))

#reference for lat,long -https://gist.github.com/rweald/4720788

library(ggplot2)
library(fiftystater)
library(dplyr)
library(reshape2)
data("fifty_states")

input <- read.csv("./data/StateDatabyWeekforMap_2017-18week3-4.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)[,1:5]
df<- input[,c(1,4)]


levels(df$STATENAME) <- tolower(levels(df$STATENAME))
DF <- df[c(1:8, 10:51),]

p <- ggplot(DF, aes(map_id = DF$STATENAME)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = DF$ACTIVITY.LEVEL), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())
p
