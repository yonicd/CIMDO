pkg=c("plyr","ggplot2","reshape2","stringr","scales","grid","gtools","shinyAce","gridExtra","mgcv","caTools","dplyr")
x=sapply(pkg,require,character.only=T,warn.conflicts = F, quietly = T)
rm(pkg,x)

setwd("H:/u243/IMF CIMDO/")

read.files=F

if(read.files){
load("RCIMDO/cimdo_description.rdata")
load("RCIMDO/security_names.rdata")

yq=function (x,prefix="%Y",combine="Q"){
  paste(ifelse(is.null(prefix),"",format(x,prefix)),floor(as.numeric(format(x,"%m"))/3-1e-3)+1,sep=combine)
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

remove_geom <- function(p, geom_old) {
  layers <- lapply(p$layers, function(x) if(x$geom$objname == geom_old) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  p$layers <- layers
  p
}

idx=c(1:10)

cimdo.files=data.frame(file=list.files("FSM/ALL",pattern = "csv",full.names = T),
                       desc=c("At least 1 defaults|Security Default",
                              "Exactly 1 defaults|Security Default",
                              "Group Default",
                              "Spillover Coefficient",
                              rep(NA,3),
                              "BSI",
                              "Emperical PoD",
                              "JPOD",
                              "row default|col default",
                              "row and col default",
                              "user definded groups",
                              "security|system",
                              "system|security"),
                       startrow=c(3,3,11,0,rep(NA,3),0,3,0,4,3,6,3,3))%>%
  mutate(file=as.character(file),
         matrix=grepl("matrix",file),
         shift=!desc%in%c("BSI","JPOD"))


cimdo.manual=read.csv(cimdo.files$file[13],header = T,skip = cimdo.files$startrow[13],row.names = NULL,stringsAsFactors = F)%>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y"),variable="Manual",file=cimdo.files$file[13],desc=as.character(cimdo.files$desc)[13])%>%
  rename(value=P.idxProb.going.default.idxGiven.default.)

cimdo.manual.names=read.csv(cimdo.files$file[13],header = F,skip = 3,nrows = 2,row.names = NULL,stringsAsFactors = F)
cimdo.manual.names=unlist(lapply(lapply(str_split(cimdo.manual.names$V1,"[&]"),function(x) gsub("[^0-9]","",x)),
                                 function(x1) paste0(as.character(sec.names$NAME_ENG)[sec.names$variable%in%x1],collapse=",")))

cimdo.manual$variable=paste0(cimdo.manual.names,collapse = "|")

cimdo.out=ddply(cimdo.files%>%filter(startrow%in%c(0,1,2,3,11)&!matrix),.(file,desc),.fun = function(df){
  p1=read.csv(df$file,header = T,skip = df$startrow,row.names = NULL)
  if(df$shift){
    nc=ncol(p1)
    names(p1)[-nc]=names(p1)[-1]
    p1=p1[,-nc]}
  p1=p1%>%mutate(Date=as.Date(Date,format="%d/%m/%Y"))%>%melt(.,id="Date")%>%
    mutate(variable=gsub("[.|P.]"," ",variable))
  return(p1)})

cimdo.out=rbind(cimdo.out%>%mutate(desc=as.character(desc)),cimdo.manual)%>%
  mutate(variable=str_trim(gsub("rate_adj","",variable)))%>%
  mutate_each(funs(factor),file,desc)

cimdo.out=cimdo.out%>%left_join(sec.names,by="variable")%>%
  mutate(
    NAME_ENG=factor(NAME_ENG,levels=sec.names$NAME_ENG[idx]),
    NAME_ENG.C=factor(NAME_ENG,labels=paste(LETTERS[idx],levels(NAME_ENG),sep=" : ")),
    LETTER=factor(NAME_ENG,labels=LETTERS[idx]),
    Y=format(Date,"%Y"),
    m=format(Date,"%m"),
    Q=yq(Date,prefix = NULL)
  )


DiDe=ddply(cimdo.files%>%filter(matrix),.(file,desc),.progress = "text",.fun = function(df){
jointmat=read.csv(df$file,header = T,skip = df$startrow,row.names = NULL)
names(jointmat)[-c(13)]=names(jointmat)[-c(1)]
jointmat=jointmat[,-13]
names(jointmat)[2]="to.default"
jointmat$Date[jointmat$Date==""]=NA
jointmat$Date=na.locf(jointmat$Date)
jointmat$Date=as.Date(jointmat$Date,format="%d/%m/%Y")
jointmat=jointmat%>%melt(.,id=c("to.default","Date"))
names(jointmat)[3]="in.default"
jointmat$in.default=factor(jointmat$in.default,labels=gsub("[a-z._ ]","",levels(jointmat$in.default)))
jointmat$to.default=factor(jointmat$to.default,labels=gsub("[a-z._ ]","",levels(jointmat$to.default)))
jointmat=jointmat%>%left_join(sec.names%>%select(in.default=variable,in.default.name=NAME_ENG,in.default.type=TYPE_ENG),by="in.default")%>%
  left_join(sec.names%>%select(to.default=variable,to.default.name=NAME_ENG,to.default.type=TYPE_ENG),by="to.default")
if(grepl('matrixco',df$file)) jointmat=jointmat[jointmat$in.default.name!=jointmat$to.default.name,]
return(jointmat)})

DiDe=DiDe%>%mutate(
    Y=format(Date,"%Y"),
    m=format(Date,"%m"),
    Q=yq(Date,prefix = NULL)
  )

rm(cimdo.manual,cimdo.manual.names,idx)

cimdo.shiny=cimdo.out[,c(4,12:14,8,9,3,1,5)]
cimdo.shiny=cimdo.shiny%>%mutate_each(funs(as.character),contains("ENG"))
cimdo.shiny[cimdo.shiny$desc%in%c("JPOD","BSI","Group Default","user definded groups"),c(5,6)]="ALL"
cimdo.shiny$variable[cimdo.shiny$NAME_ENG!="ALL"]=cimdo.shiny$NAME_ENG[cimdo.shiny$NAME_ENG!="ALL"]
cimdo.shiny$NAME_ENG=NULL
names(cimdo.shiny)[5]="TYPE"
names(cimdo.shiny)=toupper(names(cimdo.shiny))


DiDe.shiny=DiDe[,c(5,11:13,2,10,8,7,9,6)]
names(DiDe.shiny)=toupper(names(DiDe.shiny))
DiDe.shiny=DiDe.shiny%>%mutate_each(funs(as.numeric),Y,M,VALUE)%>%mutate_each(funs(factor),-c(Y,M,DATE,VALUE))
DiDe.shiny=rbind(DiDe.shiny%>%mutate(TYPE=paste0("IN.DEFAULT:",IN.DEFAULT.TYPE),VARIABLE=paste0("IN.DEFAULT:",IN.DEFAULT.NAME))%>%select(DATE:DESC,TYPE,VARIABLE,VALUE),
                  DiDe.shiny%>%mutate(TYPE=paste0("TO.DEFAULT:",TO.DEFAULT.TYPE),VARIABLE=paste0("TO.DEFAULT:",TO.DEFAULT.NAME))%>%select(DATE:DESC,TYPE,VARIABLE,VALUE))

to.shiny=rbind(cimdo.shiny,DiDe.shiny)

to.shiny=to.shiny%>%mutate_each(funs(as.numeric),Y,M)%>%mutate_each(funs(factor),-c(Y,M,DATE,VALUE))

save.image(file="Shiny/cimdoshiny.rdata")


}else{
  load("Shiny/cimdoshiny.rdata")
}
