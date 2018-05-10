#' @import shiny
#' @importFrom shinyAce aceEditor
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom plyr ddply
#' @import dplyr
#' @import ggplot2
#' @importFrom grDevices png pdf
#' @importFrom stats as.formula
#' @importFrom rlang UQ syms
cimdo_gadget<-function(obj,
                       plotHeight = 800,
                       viewerType = 'paneViewer',
                       ...){
  
  viewerDots<-list(...) 
  
  if(viewerType=='dialogViewer'){
    
    if(is.null(viewerDots$dialogName)) viewerDots$dialogName <- 'shinyCIMDO'
    
    if(is.null(viewerDots$width)) viewerDots$width <- 1600
    
    if(is.null(viewerDots$height)) viewerDots$height <- 1000
  } 
  
  viewer <- do.call(eval(parse(text=paste0('shiny::',viewerType))),viewerDots)
  
  #UI----
  ui <- shiny::shinyUI(
    shiny::fluidPage(
      
      # Application title
      shiny::headerPanel("CIMDO Stability Measure Dashboard"),
      
      # Sidebar with controls to select the variable to plot against mpg
      # and to specify whether outliers should be included
      shiny::sidebarPanel(width=3,
                          shiny::radioButtons(inputId = "ext", label = "Dashboard Type",choices = c("Basic","Advanced"),selected='Basic',inline=TRUE),
                          shiny::hr(),
                          shiny::conditionalPanel('input.plotmode=="ggplot"',uiOutput("Y"),
                                                  shiny::uiOutput("Q"),
                                                  shiny::sliderInput("M","Month",min = 1,max=12,value=c(1,12)),
                                                  shiny::hr()),
                          shiny::checkboxInput("factor",label = "Group Factor",value = FALSE),
                          shiny::checkboxInput("rotate",label = "Rotate X label",value = TRUE),
                          shiny::hr(),
                          shiny::radioButtons("facet_type",label = NULL,choices=split(c("wrap","grid"),c("Facet Wrap","Facet Grid")),selected = "grid",inline=TRUE),
                          shiny::radioButtons("facet_scales","Facet Scales",choices=c("fixed","free_x","free_y","free"),selected = "free",inline=TRUE),
                          shiny::uiOutput("facet_row"),
                          shiny::uiOutput("facet_col"),
                          shiny::conditionalPanel('input.ext=="Advanced"',
                                                  shiny::hr(),
                                                  shiny::actionButton("send","User Layer"),
                                                  shinyAce::aceEditor(outputId = "code",
                                                                      value = "p",
                                                                      mode = "r", 
                                                                      theme = "chrome", 
                                                                      height = "100px", 
                                                                      fontSize = 10))
      ),
      
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Dashboard",
                          shiny::fluidRow(
                            shiny::column(width=4,
                                          shiny::radioButtons(inputId="plotmode",label = "Plot Mode",choices=split(c("ggplot","plotly"),c("Static","Interactive")),selected = "ggplot",inline = TRUE)
                            ),
                            shiny::column(width=8,
                                          shiny::uiOutput("ptype"))
                          ),
                          shiny::fluidRow(
                            shiny::column(width=3,
                                          shiny::uiOutput("CASE")
                            ),
                            shiny::column(width=3,
                                          shiny::uiOutput("MEASURE")
                            ),
                            shiny::column(width=3,
                                          shiny::uiOutput("VARIABLE")
                            )
                          ),
                          
                          shiny::fluidRow(
                            shiny::column(width=3,shiny::uiOutput("xvar")),
                            shiny::column(width=3,shiny::uiOutput("yvar")),
                            shiny::column(width=3,shiny::uiOutput("fill"))
                          ),
                          
                          shiny::fluidRow(
                            shiny::conditionalPanel('input.plotmode=="ggplot"',
                                                    shiny::column(width=12,
                                                                  shiny::plotOutput(outputId = "CimdoPlot",
                                                                                    height = "450px")
                                                    )
                            ),
                            shiny::conditionalPanel('input.plotmode==="plotly"',
                                                    shiny::column(width=12,
                                                                  plotly::plotlyOutput(outputId = "CimdoPlotly",
                                                                                      width = "auto",
                                                                                      height = "auto"))
                            ),
                            shiny::conditionalPanel("input.plotmode==='ggplot'",
                                                    shiny::column(width=3,
                                                                  shiny::downloadButton('downloadPlot', 'Download Plot'))
                            )
                          )),
          
          shiny::tabPanel("Data",shiny::tags$head(
            shiny::tags$style("tfoot{display:table-header-group;}")
          ),
          shiny::fluidPage(
            shiny::column(12,
                          shiny::dataTableOutput('table'))
          )
          ),
          
          shiny::tabPanel("Measure Explanation",
                          shiny::tags$h3("The stability measures calculated by the CIMDO are:"),
                          shiny::tags$li("Common Distress within System"),
                          shiny::tags$ul(shiny::tags$li("Joint Probability of Distress (JPoD): Joint probabilities of all institutions going default"),
                                         shiny::tags$li("Banking stablity Index (BSI): The expected number of institutions defaults given that at least one institution defaults")
                          ),
                          shiny::tags$li("Distress Conditioned on Subset"),
                          shiny::tags$ul(shiny::tags$li("Group Default: Conditional probabilities between banks and insurance companies"),
                                         shiny::tags$li("User Defined Groups: Probability of Distress of groups defined by user")
                          ),
                          shiny::tags$li("Distress in the System Associated with a Specific Bank"),
                          shiny::tags$ul(
                            shiny::tags$li("Spillover Coefficient: Distress dependence measure that computes the probability of distress a bank conditional on others becoming distressed. This is a weighted sum of the probability of distress of bank i given the default of each other banks in the sample"),
                            shiny::tags$li("At least 1 defaults|Security Default: Prob(At least 1 institution defaults| institution  i Default)"),
                            shiny::tags$li("Exactly 1 defaults|Security Default: Prob(Exactly least 1 institution defaults| institution  i Default)"),
                            shiny::tags$li("System|Security: Prob(all other institutions going default | institution i default)"),
                            shiny::tags$li("Security|System: Prob(institution i going default | all other institutions default)")),
                          
                          shiny::tags$li("Distress Between Specific Banks"),
                          shiny::tags$ul(tags$li("row default|col default: Bivariate Conditional Probaiblity of Distress"),
                                         shiny::tags$li("row default and col default: Bivariate Joint Probaiblity of Distress")
                          ),
                          
                          shiny::tags$li("Emperical PoD: Univariate Distress measured as VaR. They the marginal probabilities of distress that are input to calculate them CIMDO")
          ),
          
          shiny::tabPanel("CIMDO Methodology",
                          shiny::tags$h2("Background"), 
                          shiny::tags$p("A set of banking stability measures which take account of distress dependence among the banks in a system, thereby providing a set of tools to analyze stability from complementary perspectives by allowing the measurement of"),
                          shiny::tags$li("common distress of the banks in a system"),
                          shiny::tags$li("distress between specific banks"),
                          shiny::tags$li("distress in the system associated with a specific bank"),
                          shiny::tags$br(),
                          shiny::tags$p("The approach defines the banking system as a portfolio of banks and infers the system's multivariate density (BSMD) from which the proposed measures are estimated. The BSMD embeds the banks' default inter-dependence structure that captures linear and non-linear distress dependencies among the banks in the system, and its changes at different times of the economic cycle. The BSMD is recovered using the CIMDO-approach, that in the presence of restricted data, improves density specification without explicitly imposing parametric forms that, under restricted data sets, are difficult to model. Thus, the proposed measures can be constructed from a very limited set of publicly available data and can be provided for a wide range of both developing and developed countries."),
                          shiny::tags$p("This methodology was dervied by ",tags$a(href="https://www.imf.org/external/pubs/ft/wp/2009/wp0904.pdf)","Segoviano and Goodhart 2009")),  
                          shiny::tags$h2("Application in major financial institutions"),
                          shiny::tags$p("The CIMDO framework and other market-based indicators are considered by many authorities an important, though not exclusive, tool to support the analysis of financial stability. The value added of this framework has been widely recognized by its use in various IMF global financial Stability Reviews (GFSRs), important Financial Sector assessment Programs (FSAPs), including the US in 2010, 2015, UK in 2011 and currently in 2016, Denmark, Egypt, Israel etc., and has been adopted by various Central Banks including the Riksbank, ECB, Banque de France, Mexico CNBV, Reserve Bank of India etc.)")
                          
          )
        )
      ) 
    ))
  #Server---- 
  
  server <- function(input, output,session) {

    output$Y <- renderUI({

      sliderInput("Y", 
                  label = "Timeline",
                  min = min(obj$Y),
                  animate = FALSE,
                  max = max(obj$Y),
                  value = range(obj$Y),
                  step = 1)
    })
    
    output$Q <- renderUI({

      checkboxGroupInput("Q", 
                         label = "Quarter",
                         choices = levels(obj$Q),
                         selected = levels(obj$Q),
                         inline=TRUE
      )
    })
    
    output$facet_row <- renderUI({

      selectInput('facet_row',
                  'Facet Row', 
                  c(None='.', names(obj)[-1],"TO.DEFAULT","IN.DEFAULT")
      )
      
    })  
    
    output$facet_col <- renderUI({
      
      selectInput('facet_col', 
                  'Facet Column', 
                  c(None='.', names(obj)[-1],"TO.DEFAULT","IN.DEFAULT")
      )
      
    })
    
    output$CASE <- renderUI({

      selectInput(inputId = "CASE",
                  label = "Data",
                  choices = levels(obj$CASE),
                  selected=levels(obj$CASE)[1],
                  multiple = TRUE
      )
      
    })
    
    output$ptype <- renderUI({
      
      ptypes <- c("line","point","stacked","density","smoothed","boxplot")
      
      if(input$ext=="Basic") 
        ptypes=c("line","point","stacked","density","smoothed")
      
      radioButtons("ptype",
                   "Plot Type",
                   choices = ptypes,
                   selected = "line",
                   inline=TRUE)
    })
    
    output$MEASURE<-renderUI({

      selectInput(inputId = "MEASURE", 
                  label = "Measure",
                  choices = levels(obj$MEASURE),
                  selected=levels(obj$MEASURE)[1],
                  multiple = TRUE)
      
    })
    
    output$VARIABLE<-renderUI({

      VARIABLE <- levels(factor(obj$VARIABLE[obj$MEASURE%in%input$MEASURE]))
      
      if(any(grepl("IN",VARIABLE))) 
        VARIABLE <- gsub("TO.DEFAULT:|IN.DEFAULT:",
                         "",
                         unique(as.character(obj$VARIABLE))
        )
      
      selectInput(inputId = "VARIABLE",
                  label =  "Measure Filter",
                  choices = VARIABLE,
                  multiple = TRUE)
    })
    
    output$xvar<-renderUI({

      selectInput('var_x', 
                  'X variable', 
                  c('None', names(obj)[-7],"TO.DEFAULT","IN.DEFAULT"),
                  selected = "DATE")
    })
    
    output$yvar<-renderUI({

      selectInput('var_y', 
                  'Y variable',
                  c('None', names(obj)[-1],"TO.DEFAULT","IN.DEFAULT"),
                  selected = "VALUE")
      
    })
    
    output$fill<-renderUI({

      selectInput('var_fill', 
                  'Group', 
                  c('None', names(obj)[-1],"TO.DEFAULT","IN.DEFAULT"),
                  selected = "VARIABLE")
      
    })
    
    data_r <- reactive({
      
      if(is.null(input$ptype))
        return(NULL)
      
      x <- obj
      
      if(length(input$CASE)>0) 
        x <- x[x$CASE%in%input$CASE,]
      
      if(length(input$Y)>0) 
        x <- x[x$Y%in%seq(input$Y[1],input$Y[2]),]
      
      if(length(input$Q)>0)
        x <- x[x$Q%in%input$Q,]
      
      if(length(input$M)>0) 
        x <- x[x$M%in%seq(input$M[1],input$M[2]),]
      
      if(length(input$VARIABLE)>0) 
        x <- x[x$VARIABLE%in%input$VARIABLE,]
      
      if(length(input$MEASURE)>0) 
        x <- x[x$MEASURE%in%input$MEASURE,]
      
      maxx <- max(x$VALUE)
      
      temp1 <- ifelse(input$var_x==as.character(legend_var[1,1]),
                      as.character(legend_var[1,2]),
                      ifelse(input$var_x==as.character(legend_var[2,1]),
                             as.character(legend_var[2,2]),
                             input$var_x)
      )
      temp2 <- ifelse(input$var_y==as.character(legend_var[1,1]),
                      as.character(legend_var[1,2]),
                      ifelse(input$var_y==as.character(legend_var[2,1]),
                             as.character(legend_var[2,2]),
                             input$var_y))
      
      if(any(input$MEASURE%in%c("row and col default","row default|col default"))){
        
        x$TO.DEFAULT <- unlist(lapply(strsplit(as.character(x$VARIABLE),"[|]"),'[',1))
        
        x$IN.DEFAULT <- unlist(lapply(strsplit(as.character(x$VARIABLE),"[|]"),'[',2))
        
      }

      p <- x%>%
        ggplot2::ggplot() +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90*as.numeric(input$rotate),hjust=1))
      
      if(input$ptype=="line") 
        p <- p + 
        ggplot2::geom_line(ggplot2::aes(x=rlang::UQ(rlang::sym(input$var_x)),
                                        y=rlang::UQ(rlang::sym(input$var_y))))
      
      if(input$ptype=="point") 
        p <- p + 
        ggplot2::geom_point(ggplot2::aes(x=rlang::UQ(rlang::sym(input$var_x)),
                                         y=rlang::UQ(rlang::sym(input$var_y))))
      
      if(input$ptype=="boxplot") 
        p <- p + 
        ggplot2::geom_boxplot(ggplot2::aes(x=factor(rlang::UQ(rlang::sym(input$var_x))),
                                           y=rlang::UQ(rlang::sym(input$var_y))))
      
      if(input$ptype=="density") 
        p <- p + 
        #scales density plots to total 1 (makes them pdfs)
        ggplot2::geom_density(ggplot2::aes(x=rlang::UQ(rlang::sym(input$var_y)),
                                           y=..scaled..),
                              alpha=.25) 
      
      if(input$ptype=="stacked") 
        p <- p + 
        ggplot2::geom_ribbon(ggplot2::aes(x = rlang::UQ(rlang::sym(input$var_x)),
                                          y = rlang::UQ(rlang::sym(input$var_y)),
                                          ymin = 0,
                                          ymax = maxx,
                                          group = rlang::UQ(rlang::sym(input$var_fill)),
                                          fill = rlang::UQ(rlang::sym(input$var_fill))),
                             position = ggplot2::position_stack())
      
      if(input$ptype=="smoothed") 
        p <- p + 
        ggplot2::geom_smooth(ggplot2::aes(x = rlang::UQ(rlang::sym(input$var_x)) ,
                                          y = rlang::UQ(rlang::sym(input$var_y))))
      
      if (input$var_fill != 'None'){

        if(input$ptype%in%c("line","point","smoothed")){
          
          if(input$factor){
            p <-  p + ggplot2::aes(color = factor(rlang::UQ(rlang::sym(input$var_fill)))) + 
                      ggplot2::scale_color_discrete(name = input$var_fill)
          }else{
            p <-  p + ggplot2::aes(color = rlang::UQ(rlang::sym(input$var_fill)))
          }

        
          }else{ 
            if(input$ptype%in%c("boxplot","density")){

            if(input$factor){
              p <-  ggplot2::aes(fill = factor(rlang::UQ(rlang::sym(input$var_fill)))) +
                    ggplot2::scale_fill_discrete(name=input$var_fill)
              
            }else{
              
              p <-  p + ggplot2::aes(fill = rlang::UQ(rlang::sym(input$var_fill)))
              
            }  
           
          }
          }
        
      }
      
      if( input$facet_type=="wrap" & (input$facet_row!="."|input$facet_col!=".")){
        
        if(input$facet_row!=".") 
          facets <- stats::as.formula(paste('~', input$facet_row))
        
        if(input$facet_col!=".") 
          facets <- stats::as.formula(paste('~', input$facet_col))
        
        if(input$facet_col!="."&input$facet_col!=".") 
          facets <- stats::as.formula(paste('~', input$facet_row,"+",input$facet_col))
        
        p <- p + 
          ggplot2::facet_wrap(facets,scales = input$facet_scales)
        
      }else{
        
        facets <- paste(input$facet_row, '~', input$facet_col)
        
        if (facets != '. ~ .')
          p <- p + 
            ggplot2::facet_grid(facets,
                                labeller = ggplot2::label_both,
                                scales = input$facet_scales) 
        
      }
      
      p <- p+
        ggplot2::labs(x = temp1,
                      y = temp2)
      
      return(p)     
    })
    
    output$table <- renderDataTable({
      
      obj
      
    })   
    
    output$downloadPlot <- 
      downloadHandler(filename = "CIMDOPLOT.png",
                      content = function(file){
                        p <- data_r()
                        device = function(...,width,height){
                          grDevices::png(...,width,height,res=300,units="in")
                        }
                        ggplot2::ggsave(file,
                                        plot=eval(parse(text=input$code)),
                                        device = device)
                      })
    
    observeEvent(data_r(),{
      
    output$CimdoPlot <- renderPlot(expr = {
      
      p <- data_r()
      
      input$send
      
      isolate({
        eval(parse(text = input$code))
      })
      
    })

    output$CimdoPlotly <- plotly::renderPlotly(expr = {
        
        grDevices::pdf(NULL)
        
        p <- data_r()
        
        input$send
        
        isolate({
          eval(parse(text = input$code))
        })
        
      })
    })
    
  }
  
  shiny::runGadget(ui, server, viewer = viewer)
  
}