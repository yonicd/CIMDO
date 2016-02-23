shinyServer(function(input, output, session) {

#   output$DESC<-renderUI({
#     DESC=levels(to.shiny[[input$df]]$DESC)
#     selectInput(inputId = "DESC", label = "Measure",choices = DESC,selected=DESC[1])
#   })      
  
  output$VARIABLE<-renderUI({
    VARIABLE=levels(factor(to.shiny$VARIABLE[to.shiny$DESC%in%input$DESC]))
    if(any(grepl("IN",VARIABLE))) VARIABLE=gsub("TO.DEFAULT:|IN.DEFAULT:","",unique(as.character(to.shiny$VARIABLE)))
    selectInput(inputId = "VARIABLE",label =  "Institutions Filter",choices = VARIABLE,multiple = T)
  })
  
      data.r=reactive({
	     x=to.shiny
      
      if(length(input$Y)>0) x=x%>%filter(Y%in%seq(input$Y[1],input$Y[2]))
      if(length(input$Q)>0) x=x%>%filter(Q%in%input$Q)
      if(length(input$M)>0) x=x%>%filter(M%in%seq(input$M[1],input$M[2]))
      if(length(input$VARIABLE)>0) x=x%>%filter(VARIABLE%in%input$VARIABLE)
      if(length(input$DESC)>0) x=x%>%filter(DESC%in%input$DESC)

      maxx=max(x$VALUE)
      temp1=ifelse(input$var_x==as.character(leg.var[1,1]),as.character(leg.var[1,2]),ifelse(input$var_x==as.character(leg.var[2,1]),as.character(leg.var[2,2]),input$var_x))
      temp2=ifelse(input$var_y==as.character(leg.var[1,1]),as.character(leg.var[1,2]),ifelse(input$var_y==as.character(leg.var[2,1]),as.character(leg.var[2,2]),input$var_y))
      
      if(input$DESC%in%c("row and col default","row default|col default")){
        x2=x%>%filter(grepl("TO",VARIABLE))%>%rename(TO.DEFAULT=VARIABLE)%>%mutate_each(funs(gsub("TO.DEFAULT:","",.)),TO.DEFAULT,TYPE)
        x3=x%>%filter(grepl("IN",VARIABLE))%>%mutate(VARIABLE=gsub("IN.DEFAULT:","",VARIABLE))%>%select(-TYPE)
        x=x2%>%left_join(x3,by=names(x2)[-c(5,7)])
        rm(x2,x3)
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
        p = p + aes_string(colour=filltxt)
        if(input$factor) p=p+scale_colour_discrete(name=input$var_fill)
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
  
    p=p+xlab(temp1)+ylab(temp2)+ggtitle(input$DESC)
    
#     if(input$ptype=="smoothed" & input$output_df=="Shapley Value for VaR0.99"){
#       xx1=x%>%filter(x$TYPE=="banks")
#       xx2=x%>%filter(x$TYPE=="insurance")
#       p1=ggplot(xx1)+geom_smooth(aes_string(x=input$var_x,y=input$var_y,color=input$var_fill),fill=NA)+geom_hline(yintercept=1)+theme_bw()
#       p2=ggplot(xx2)+geom_smooth(aes_string(x=input$var_x,y=input$var_y,color=input$var_fill),fill=NA)+geom_hline(yintercept=1)+theme_bw()
#       p=grid.arrange(p1,p2,ncol=2)
#     }

    return(p)     
  })

  output$table=renderDataTable(to.shiny)   
  
  output$downloadPlot=downloadHandler(filename="CIMDOPLOT.png",
                                      content=function(file){
                                        p=data.r()
                                        device=function(...,width,height) grDevices::png(...,width,height,res=300,units="in")
                                        ggsave(file,plot=eval(parse(text=input$code)),device = device)
                                      })
      
  output$CimdoPlot=renderPlot({
    p=data.r()
    input$send
    isolate({
      print(eval(parse(text = input$code)))
    })
  }) 
  
})
