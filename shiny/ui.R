
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("CIMDO FSM Dashboard"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(width=3,
#    selectInput("df", "Measure Type",names(to.shiny),selected=names(to.shiny)[1]),
    selectInput(inputId = "DESC", label = "Measure",choices = levels(to.shiny$DESC),selected=levels(to.shiny$DESC)[1],multiple = T),
    uiOutput("VARIABLE"),
    sliderInput("Y", label = NULL,min = min(to.shiny$Y),animate = T,
                max = max(to.shiny$Y),value = range(to.shiny$Y),step = 1),
    checkboxGroupInput("Q", label = NULL,choices = levels(to.shiny$Q),selected = levels(to.shiny$Q),inline=T),
    sliderInput("M","Month",min = 1,max=12,value=c(1,12)),
    br(),
    radioButtons("ptype", "Plot Type",choices = c("line","point","boxplot","density","stacked","smoothed"),selected = "line",inline=T),
    selectInput('var_x', 'X variable', c('None', names(to.shiny)[-8],"TO.DEFAULT"),selected = "DATE"),
    selectInput('var_y', 'Y variable', c('None', names(to.shiny)[-1],"TO.DEFAULT"),selected = "VALUE"),
    selectInput('var_fill', 'Group', c('None', names(to.shiny)[-1],"TO.DEFAULT"),selected = "VARIABLE"),
    checkboxInput("factor",label = "Group Factor",value = F),checkboxInput("rotate",label = "Rotate X label",value = T),
    radioButtons("facet_type",label = NULL,choices=split(c("wrap","grid"),c("Facet Wrap","Facet Grid")),selected = "wrap",inline=T),
    radioButtons("facet_scales","Facet Scales",choices=c("fixed","free_x","free_y","free"),selected = "free_x",inline=T),
    selectInput('facet_row', 'Facet Row', c(None='.', names(to.shiny)[-1],"TO.DEFAULT")),
    selectInput('facet_col', 'Facet Column', c(None='.', names(to.shiny)[-1],"TO.DEFAULT")),
    #downloadButton('foo', 'Download Plot')
    fluidRow(
      column(width = 8,
             actionButton("send","User Layer"),
             aceEditor(outputId = "code",
                       value = "p",
                       mode = "r", theme = "chrome", height = "100px", fontSize = 10)
             #column(8,textInput("usr.input",label = NULL,value = "geom_point(aes(x=Date,y=value))"))  
      )
    )
  ),

mainPanel(

 tabsetPanel(
   tabPanel("Dashboard",
            fluidRow(
              downloadButton('downloadPlot', 'Download Plot'),
              hr(),
              plotOutput(outputId = "CimdoPlot",height ="800px")
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
