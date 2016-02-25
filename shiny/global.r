pkg=c("plyr","zoo","ggplot2","reshape2","stringr","scales","grid","shinyAce","gridExtra","plotly","shiny","dplyr")
x=sapply(pkg,require,character.only=T,warn.conflicts = F, quietly = T)
rm(pkg,x)

read.files=T

remove_geom <- function(p, geom) {
  layers <- lapply(p$layers, function(x) if(any(grepl(paste0('(?i)',geom),class(x$geom)))) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  
  p$layers <- layers
  p
}

read.cimdo=function(file.path){
  
  load("cimdokey.rdata")
  
  yq=function (x,prefix="%Y",combine="Q"){
    paste(ifelse(is.null(prefix),"",format(x,prefix)),floor(as.numeric(format(x,"%m"))/3-1e-3)+1,sep=combine)
  }
  
cimdo.files=data.frame(file=list.files(file.path,pattern = "csv",full.names = T),
                       name=list.files(file.path,pattern = "csv",full.names = F),stringsAsFactors = F)%>%
  left_join(cimdo.key,by="name")

cimdo.files=cimdo.files[which(sapply(cimdo.files$file,file.size)!=0),]

file.id=which(cimdo.files$desc=="user definded groups")

cimdo.manual=read.csv(cimdo.files$file[file.id],header = T,skip = cimdo.files$startrow[file.id],row.names = NULL,stringsAsFactors = F)%>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y"),variable="Manual",file=cimdo.files$file[file.id],desc=as.character(cimdo.files$desc)[file.id])%>%
  rename(value=P.idxProb.going.default.idxGiven.default.)

cimdo.manual.names=read.csv(cimdo.files$file[file.id],header = F,skip = 3,nrows = 2,row.names = NULL,stringsAsFactors = F)
  cimdo.manual.names=gsub("[()]|idxProb=|idxGiven=","",str_trim(cimdo.manual.names$V1))
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

cimdo.out=cimdo.out%>%
  #left_join(sec.names,by="variable")%>%
  mutate(
#     NAME_ENG=factor(NAME_ENG,levels=sec.names$NAME_ENG[idx]),
#     NAME_ENG.C=factor(NAME_ENG,labels=paste(LETTERS[idx],levels(NAME_ENG),sep=" : ")),
#     LETTER=factor(NAME_ENG,labels=LETTERS[idx]),
    Y=format(Date,"%Y"),
    m=format(Date,"%m"),
    Q=yq(Date,prefix = NULL)
  )


DiDe=ddply(cimdo.files%>%filter(matrix),.(file,desc),.fun = function(df){
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

cimdo.shiny=cimdo.out[,c("Date","Y","Q","m","variable","desc","value")]
names(cimdo.shiny)=toupper(names(cimdo.shiny))


DiDe.shiny=DiDe[,c("Date","Y","Q","m","desc","in.default","to.default","value")]
names(DiDe.shiny)=toupper(names(DiDe.shiny))
DiDe.shiny=DiDe.shiny%>%mutate(VARIABLE=paste(TO.DEFAULT,IN.DEFAULT,sep="|"))%>%select(-c(TO.DEFAULT,IN.DEFAULT))
DiDe.shiny=DiDe.shiny%>%mutate_each(funs(as.numeric),Y,M,VALUE)%>%mutate_each(funs(factor),-c(Y,M,DATE,VALUE))

to.shiny=rbind(cimdo.shiny,DiDe.shiny)

to.shiny=to.shiny%>%mutate_each(funs(as.numeric),Y,M)%>%mutate_each(funs(factor),-c(Y,M,DATE,VALUE))

to.shiny$DESC=factor(to.shiny$DESC,levels=as.character(cimdo.key$desc)[as.character(cimdo.key$desc)%in%levels(to.shiny$DESC)])
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

# data.type="Patient"
# case=data.frame(CASE=unlist(lapply(strsplit(list.dirs(paste0("FSM/",data.type))[-1],"/"),'[',3)))
# to.shiny=ddply(case,.(CASE),.fun = function(x) read.cimdo(paste("FSM",data.type,x$CASE,sep="/")))
# to.shiny=to.shiny[,names(to.shiny)[c(2:8,1)]]