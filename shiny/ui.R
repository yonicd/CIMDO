
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("CIMDO FSM Dashboard"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(width=3,
               #column(width=3,selectInput(inputId = "data.type", label = "Data Directory",choices = c("Patient","Finance"),selected='Patient')),               
               sliderInput("Y", label = "Timeline",min = min(to.shiny$Y),animate = F,max = max(to.shiny$Y),value = range(to.shiny$Y),step = 1),
               checkboxGroupInput("Q", label = "Quarter",choices = levels(to.shiny$Q),selected = levels(to.shiny$Q),inline=T),
               sliderInput("M","Month",min = 1,max=12,value=c(1,12)),
               checkboxInput("factor",label = "Group Factor",value = F),
               checkboxInput("rotate",label = "Rotate X label",value = T),
               radioButtons("facet_type",label = NULL,choices=split(c("wrap","grid"),c("Facet Wrap","Facet Grid")),selected = "wrap",inline=T),
               radioButtons("facet_scales","Facet Scales",choices=c("fixed","free_x","free_y","free"),selected = "free_x",inline=T),
               selectInput('facet_row', 'Facet Row', c(None='.', names(to.shiny)[-1],"TO.DEFAULT","IN.DEFAULT")),
               selectInput('facet_col', 'Facet Column', c(None='.', names(to.shiny)[-1],"TO.DEFAULT","IN.DEFAULT")),
                      actionButton("send","User Layer"),
                      aceEditor(outputId = "code",
                                value = "p",
                                mode = "r", theme = "chrome", height = "100px", fontSize = 10)
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Dashboard",
               fluidRow(
                 column(width=4,radioButtons(inputId="plotmode",label = "Plot Mode",choices=split(c("ggplot","plotly"),c("Static","Interactive (Beta)")),selected = "ggplot",inline = T)),
                 column(width=8,radioButtons("ptype", "Plot Type",choices = c("line","point","boxplot","density","stacked","smoothed"),selected = "line",inline=T))),
               fluidRow(
                 column(width=3,selectInput(inputId = "CASE", label = "Data",choices = levels(to.shiny$CASE),selected=levels(to.shiny$CASE)[1],multiple = T)),
                 column(width=3,selectInput(inputId = "DESC", label = "Measure",choices = levels(to.shiny$DESC),selected=levels(to.shiny$DESC)[1],multiple = T)),
                 column(width=3,uiOutput("VARIABLE"))),
               fluidRow(
                 column(width=3,selectInput('var_x', 'X variable', c('None', names(to.shiny)[-7],"TO.DEFAULT","IN.DEFAULT"),selected = "DATE")),
                 column(width=3,selectInput('var_y', 'Y variable', c('None', names(to.shiny)[-1],"TO.DEFAULT","IN.DEFAULT"),selected = "VALUE")),
                 column(width=3,selectInput('var_fill', 'Group', c('None', names(to.shiny)[-1],"TO.DEFAULT","IN.DEFAULT"),selected = "CASE"))
               ),
               fluidRow(
                 conditionalPanel('input.plotmode=="ggplot"',
                 column(width=12,plotOutput(outputId = "CimdoPlot",height = "450px"))
                 ),
                 conditionalPanel('input.plotmode==="plotly"',
                 column(width=12,plotlyOutput(outputId = "CimdoPlotly"))
                 ),
               conditionalPanel("input.plotmode==='Static'",
               column(width=3,downloadButton('downloadPlot', 'Download Plot')))
               )),
      tabPanel("Explanation of the outputs",h6("The outputs are:"),
               h6("JPoD: Joint probabilities of all institutions going default"),
               h6("BSI: The expected number of institutions defaults given that at least one institution defaults"),
               h6("QuadProb: Conditional probabilities between banks and insurance companies"),
               h6("system|security: Prob(all other institutions going default | institution i default),"),
               h6("security|system: Prob(institution i going default | all other institutions default),"),
               #h6("OTHERSgivenONE by OnegivenAllother: X-Axis is Prob(all other institutions going default | institution i default) and Y-Axis is Prob(institution i going default | all other institutions default),"),
               h6("DiDe: Each one of the 10 plots contains the conditional probability of bank i=1,...,10 going default given that bank j default")),
      
      tabPanel("Data",tags$head(tags$style("tfoot{display:table-header-group;}")),
               fluidPage(
                 fluidRow(
                   column(12,
                          dataTableOutput('table'))
                 )
               )) 
    )
  ) 
  
))
