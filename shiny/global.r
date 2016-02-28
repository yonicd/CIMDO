library(plyr)
library(zoo)
library(ggplot2)
library(reshape2)
library(stringr)
library(scales)
library(grid)
library(gridExtra)
library(labeling)
library(plotly)
library(shiny)
library(shinyAce)
library(dplyr)

remove_geom <- function(p, geom) {
  layers <- lapply(p$layers, function(x) if(any(grepl(paste0('(?i)',geom),class(x$geom)))) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  
  p$layers <- layers
  p
}

read.cimdo=function(file.path,data.type){
  
  load("cimdokey.rdata")
  
  yq=function (x,prefix="%Y",combine="Q"){
    paste(ifelse(is.null(prefix),"",format(x,prefix)),floor(as.numeric(format(x,"%m"))/3-1e-3)+1,sep=combine)
  }
  
cimdo.files=data.frame(file=list.files(file.path,pattern = "csv",full.names = T),
                       name=list.files(file.path,pattern = "csv",full.names = F),stringsAsFactors = F)%>%
  left_join(cimdo.key,by="name")

cimdo.files=cimdo.files[which(sapply(cimdo.files$file,file.size)!=0),]

file.id=which(cimdo.files$measure=="user definded groups")

cimdo.manual=read.csv(cimdo.files$file[file.id],header = T,skip = cimdo.files$startrow[file.id],row.names = NULL,stringsAsFactors = F)%>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y"),variable="Manual",file=cimdo.files$file[file.id],measure=as.character(cimdo.files$measure)[file.id])%>%
  rename(value=P.idxProb.going.default.idxGiven.default.)

cimdo.manual.names=read.csv(cimdo.files$file[file.id],header = F,skip = 3,nrows = 2,row.names = NULL,stringsAsFactors = F)
  cimdo.manual.names=gsub("[()]|idxProb=|idxGiven=","",str_trim(cimdo.manual.names$V1))
cimdo.manual$variable=paste0(cimdo.manual.names,collapse = "|")


cimdo.out=ddply(cimdo.files%>%filter(startrow%in%c(0,1,2,3,11)&!matrix),.(file,measure),.fun = function(df){
  p1=read.csv(df$file,header = T,skip = df$startrow,row.names = NULL,stringsAsFactors = F)

  if(df$shift){
    nc=ncol(p1)
    names(p1)[-nc]=names(p1)[-1]
    p1=p1[,-nc]
  }
  
  if(data.type=="Patient"){
  if(!df$measure%in%c("Raw Data","row default|col default","row and col default","user defined groups")) p1$Date=paste(p1$Date,"00:00:00")
  p1=p1%>%mutate(Date=as.POSIXct(Date,format="%d/%m/%Y %T"))
  }else{
    p1=p1%>%mutate(Date=as.Date(Date,format="%d/%m/%Y"))  
  }
  p1=p1%>%melt(.,id="Date")%>%mutate(variable=gsub("[.|P.]"," ",variable))
  return(p1)})

cimdo.out=rbind(cimdo.out%>%mutate(measure=as.character(measure)),cimdo.manual)%>%
  mutate(variable=str_trim(gsub("rate_adj","",variable)))%>%
  mutate_each(funs(factor),file,measure)

cimdo.out=cimdo.out%>%
  mutate(
    Y=format(Date,"%Y"),
    m=format(Date,"%m"),
    Q=yq(Date,prefix = NULL)
  )


DiDe=ddply(cimdo.files%>%filter(matrix),.(file,measure),.fun = function(df){
jointmat=read.csv(df$file,header = T,skip = df$startrow,row.names = NULL)
nc=ncol(jointmat)
names(jointmat)[-nc]=names(jointmat)[-c(1)]
jointmat=jointmat[,-nc]
names(jointmat)[2]="to.default"
jointmat$Date[jointmat$Date==""]=NA
jointmat$Date=na.locf(jointmat$Date)
jointmat$Date=as.Date(jointmat$Date,format="%d/%m/%Y")
jointmat=jointmat%>%melt(.,id=c("to.default","Date"))
names(jointmat)[3]="in.default"
jointmat$in.default=factor(jointmat$in.default,labels=gsub("[a-z._ ]","",levels(jointmat$in.default)))
jointmat$to.default=factor(jointmat$to.default,labels=gsub("[a-z._ ]","",levels(jointmat$to.default)))
# jointmat=jointmat%>%left_join(sec.names%>%select(in.default=variable,in.default.name=NAME_ENG,in.default.type=TYPE_ENG),by="in.default")%>%
#   left_join(sec.names%>%select(to.default=variable,to.default.name=NAME_ENG,to.default.type=TYPE_ENG),by="to.default")
jointmat=jointmat[jointmat$in.default!=jointmat$to.default,]
return(jointmat)})

DiDe=DiDe%>%mutate(
    Y=format(Date,"%Y"),
    m=format(Date,"%m"),
    Q=yq(Date,prefix = NULL)
  )

rm(cimdo.manual,cimdo.manual.names)

cimdo.shiny=cimdo.out[,c("Date","Y","Q","m","variable","measure","value")]
names(cimdo.shiny)=toupper(names(cimdo.shiny))


DiDe.shiny=DiDe[,c("Date","Y","Q","m","measure","in.default","to.default","value")]
names(DiDe.shiny)=toupper(names(DiDe.shiny))
DiDe.shiny=DiDe.shiny%>%mutate(VARIABLE=paste(TO.DEFAULT,IN.DEFAULT,sep="|"))%>%select(-c(TO.DEFAULT,IN.DEFAULT))
DiDe.shiny=DiDe.shiny%>%mutate_each(funs(as.numeric),Y,M,VALUE)%>%mutate_each(funs(factor),-c(Y,M,DATE,VALUE))

to.shiny=rbind(cimdo.shiny,DiDe.shiny)

to.shiny=to.shiny%>%mutate_each(funs(as.numeric),Y,M)%>%mutate_each(funs(factor),-c(Y,M,DATE,VALUE))

to.shiny$MEASURE=factor(to.shiny$MEASURE,levels=as.character(cimdo.key$measure)[as.character(cimdo.key$measure)%in%levels(to.shiny$MEASURE)])
return(to.shiny)
}

#### translate short variable names to long names
leg.var=data.frame(short=c("OTHERSgivenONE","OnegivenALLothers"),
                   long=c("Prob(all other banks going default | bank i default)",
                          "Prob(bank i going default | all other banks default)"))

leg.tbl=data.frame(short=c("PoDTBSI","PoDTJointProb","atLeastOneInsurGivenAllBanks",
                           "atLeastOneBankGivenAllInsur","allInsurGivenAllBanks",
                           "allBanksGivenAllInsur"),
                   long=c("expected number of bank defaults given that\r\n at least one bank defaults",
                          "joint probabilities of all banks going default",
                          "Prob(at least one insurance company going\r\n default | all banks default)",
                          "Prob(at least one bank going default | all\r\n insurance companies default)",
                          "Prob(all insurance companies going default | all\r\n banks default)",
                          "Prob(all banks  going default | all\r\n insurance companies default)"))