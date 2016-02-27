libs=c("plyr","zoo","ggplot2","reshape2","stringr","scales","grid","gridExtra","plotly","shiny","shinyAce","dplyr")
x=sapply(libs,function(x)if(!require(x,character.only = T)) install.packages(x));rm(x,libs)

library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)

load("c:/temp/physician_scores.rdata")

physician.scores.wide=physician.scores%>%dcast(CASE+CASE.TIME~NAME,value.var="SCORE",fun.aggregate = max)

physician.scores%>%
  ggplot(aes(x=CASE.TIME,y=SCORE))+
  stat_summary(fun.y=median,geom="line")+
  geom_hline(aes(yintercept=M),data=physician.scores%>%group_by(CASE)%>%summarise(M=mean(OVERALLSCORE)),linetype=2)+
  scale_y_continuous(limits=c(0,10),breaks=seq(0,10,1))+
  facet_wrap(~CASE,scales="free_x")

View(physician.scores.wide)