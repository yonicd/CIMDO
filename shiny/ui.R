to.shiny=uiOutput("to.shiny")

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("CIMDO FSM Dashboard"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(width=3,
               selectInput(inputId = "data.type", label = "Data Directory",choices = c("Patient","Finance"),selected='Patient'),
               uiOutput("Y"),
               uiOutput("Q"),
               sliderInput("M","Month",min = 1,max=12,value=c(1,12)),
               checkboxInput("factor",label = "Group Factor",value = F),
               checkboxInput("rotate",label = "Rotate X label",value = T),
               radioButtons("facet_type",label = NULL,choices=split(c("wrap","grid"),c("Facet Wrap","Facet Grid")),selected = "wrap",inline=T),
               radioButtons("facet_scales","Facet Scales",choices=c("fixed","free_x","free_y","free"),selected = "free_x",inline=T),
               uiOutput("facet.row"),
               uiOutput("facet.col"),
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
                 column(width=3,uiOutput("CASE")),
                 column(width=3,uiOutput("DESC")),
                 column(width=3,uiOutput("VARIABLE"))),
               fluidRow(
                 column(width=3,uiOutput("xvar")),
                 column(width=3,uiOutput("yvar")),
                 column(width=3,uiOutput("fill"))
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
