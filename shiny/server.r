shinyServer(function(input, output, session) {

  to.shiny<-reactive({
    data.type<-input$data.type
    if(is.null(data.type)) return(NULL)
    case=data.frame(CASE=unlist(lapply(strsplit(list.dirs(paste0("FSM/",data.type))[-1],"/"),'[',3)))
    to.shiny=ddply(case,.(CASE),.fun = function(x) read.cimdo(paste("FSM",data.type,x$CASE,sep="/")))
    to.shiny=to.shiny[,names(to.shiny)[c(2:8,1)]]
    return(to.shiny)
  })

  output$Y<-renderUI({
    df=to.shiny()
    sliderInput("Y", label = "Timeline",min = min(df$Y),animate = F,max = max(df$Y),value = range(df$Y),step = 1)
  })
  
  output$Q<-renderUI({
    df=to.shiny()
    checkboxGroupInput("Q", label = "Quarter",choices = levels(df$Q),selected = levels(df$Q),inline=T)
  })
  
output$facet.row<-renderUI({
  df=to.shiny()
  selectInput('facet_row', 'Facet Row', c(None='.', names(df)[-1],"TO.DEFAULT","IN.DEFAULT"))
})  

output$facet.col<-renderUI({
  df=to.shiny()
  selectInput('facet_col', 'Facet Column', c(None='.', names(df)[-1],"TO.DEFAULT","IN.DEFAULT"))
})

output$CASE<-renderUI({
  df=to.shiny()
  selectInput(inputId = "CASE", label = "Data",choices = levels(df$CASE),selected=levels(df$CASE)[1],multiple = T)
})

output$DESC<-renderUI({
  df=to.shiny()
  selectInput(inputId = "DESC", label = "Measure",choices = levels(df$DESC),selected=levels(df$DESC)[1],multiple = T)
})
  
output$VARIABLE<-renderUI({
  df=to.shiny()
  VARIABLE=levels(factor(df$VARIABLE[df$DESC%in%input$DESC]))
  if(any(grepl("IN",VARIABLE))) VARIABLE=gsub("TO.DEFAULT:|IN.DEFAULT:","",unique(as.character(df$VARIABLE)))
  selectInput(inputId = "VARIABLE",label =  "Measure Filter",choices = VARIABLE,multiple = T)
})

output$xvar<-renderUI({
  df=to.shiny()
  selectInput('var_x', 'X variable', c('None', names(df)[-7],"TO.DEFAULT","IN.DEFAULT"),selected = "DATE")
})

output$yvar<-renderUI({
  df=to.shiny()
  selectInput('var_y', 'Y variable', c('None', names(df)[-1],"TO.DEFAULT","IN.DEFAULT"),selected = "VALUE")
})

output$fill<-renderUI({
  df=to.shiny()
  selectInput('var_fill', 'Group', c('None', names(df)[-1],"TO.DEFAULT","IN.DEFAULT"),selected = "CASE")
})


  
      data.r=reactive({
	     x=to.shiny()
      
	    if(length(input$CASE)>0) x=x%>%filter(CASE%in%input$CASE)
      if(length(input$Y)>0) x=x%>%filter(Y%in%seq(input$Y[1],input$Y[2]))
      if(length(input$Q)>0) x=x%>%filter(Q%in%input$Q)
      if(length(input$M)>0) x=x%>%filter(M%in%seq(input$M[1],input$M[2]))
      if(length(input$VARIABLE)>0) x=x%>%filter(VARIABLE%in%input$VARIABLE)
      if(length(input$DESC)>0) x=x%>%filter(DESC%in%input$DESC)

      maxx=max(x$VALUE)
      temp1=ifelse(input$var_x==as.character(leg.var[1,1]),as.character(leg.var[1,2]),ifelse(input$var_x==as.character(leg.var[2,1]),as.character(leg.var[2,2]),input$var_x))
      temp2=ifelse(input$var_y==as.character(leg.var[1,1]),as.character(leg.var[1,2]),ifelse(input$var_y==as.character(leg.var[2,1]),as.character(leg.var[2,2]),input$var_y))
      
      if(any(input$DESC%in%c("row and col default","row default|col default"))){
        x$TO.DEFAULT=unlist(lapply(strsplit(as.character(x$VARIABLE),"[|]"),'[',1))
        x$IN.DEFAULT=unlist(lapply(strsplit(as.character(x$VARIABLE),"[|]"),'[',2))
      }
          
    p=x%>%ggplot()+theme_bw()+theme(axis.text.x=element_text(angle=90*as.numeric(input$rotate),hjust=1))

    if(input$ptype=="line") p=p+geom_line(aes_string(x=input$var_x,y=input$var_y))
    if(input$ptype=="point") p=p+geom_point(aes_string(x=input$var_x,y=input$var_y))
    if(input$ptype=="boxplot") p=p+geom_boxplot(aes_string(x=paste0("factor(",input$var_x,")"),y=input$var_y))
    if(input$ptype=="density") p=p+geom_density(aes_string(x=input$var_y,y="..scaled.."),alpha=.25) #scales density plots to total 1 (makes them pdfs)
    if(input$ptype=="stacked") p=p+geom_ribbon(aes_string(x=input$var_x,y=input$var_y,
          ymin=0,ymax=maxx,group=input$var_fill,fill=input$var_fill),position=position_stack())
    if(input$ptype=="smoothed") p=p+geom_smooth(aes_string(x=input$var_x,y=input$var_y))
     
    if (input$var_fill != 'None'){
      filltxt=ifelse(input$factor,paste0("factor(",input$var_fill,")"),input$var_fill)
      
      if(input$ptype%in%c("line","point","smoothed")){
        p = p + aes_string(color=filltxt)
        if(input$factor) p=p+scale_color_discrete(name=input$var_fill)
      }
    else if(input$ptype%in%c("boxplot","density")){
        p = p + aes_string(fill=filltxt)
        if(input$factor) p+scale_fill_discrete(name=input$var_fill)
      }}
    
    if(input$facet_type=="wrap"&(input$facet_row!="."|input$facet_col!=".")){
      if(input$facet_row!=".") facets=as.formula(paste('~', input$facet_row))
      if(input$facet_col!=".") facets=as.formula(paste('~', input$facet_col))
      if(input$facet_col!="."&input$facet_col!=".") facets=as.formula(paste('~', input$facet_row,"+",input$facet_col))
      p = p + facet_wrap(facets,scales=input$facet_scales)
    }else{
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .')
        p = p + facet_grid(facets,labeller = label_both,scales=input$facet_scales) 
      
    }
  
    p=p+xlab(temp1)+ylab(temp2)
    return(p)     
  })

  output$table=renderDataTable(to.shiny)   
  
  output$CimdoPlot=renderPlot(expr = {
    p=data.r()
    input$send
    isolate({
      eval(parse(text = input$code))
    })
  })
  
    output$downloadPlot=downloadHandler(filename="CIMDOPLOT.png",
                                        content=function(file){
                                          p=data.r()
                                          device=function(...,width,height) grDevices::png(...,width,height,res=300,units="in")
                                          ggsave(file,plot=eval(parse(text=input$code)),device = device)
                                        })
  
  output$CimdoPlotly=renderPlotly(expr = {
      p=data.r()
      input$send
      isolate({
        eval(parse(text = input$code))
      })
    })
  
})
