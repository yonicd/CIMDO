to.shiny=uiOutput("to.shiny")

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("CIMDO Stability Measure Dashboard"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(width=3,
               selectInput(inputId = "data.type", label = "Data Directory",choices = c("Patient","Finance"),selected='Finance'),
               radioButtons(inputId = "ext", label = "Dashboard Type",choices = c("Basic","Advanced"),selected='Basic',inline=T),
               hr(),
               conditionalPanel('input.plotmode=="ggplot"',uiOutput("Y"),
               uiOutput("Q"),
               sliderInput("M","Month",min = 1,max=12,value=c(1,12)),
               hr()),
               checkboxInput("factor",label = "Group Factor",value = F),
                        checkboxInput("rotate",label = "Rotate X label",value = T),
               hr(),
               radioButtons("facet_type",label = NULL,choices=split(c("wrap","grid"),c("Facet Wrap","Facet Grid")),selected = "grid",inline=T),
               radioButtons("facet_scales","Facet Scales",choices=c("fixed","free_x","free_y","free"),selected = "free",inline=T),
               uiOutput("facet.row"),
               uiOutput("facet.col"),
               conditionalPanel('input.ext=="Advanced"',
               hr(),
               actionButton("send","User Layer"),
               shinyAce::aceEditor(outputId = "code",
                                value = "p",
                                mode = "r", theme = "chrome", height = "100px", fontSize = 10))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Dashboard",
               fluidRow(
                 column(width=4,radioButtons(inputId="plotmode",label = "Plot Mode",choices=split(c("ggplot","plotly"),c("Static","Interactive")),selected = "plotly",inline = T)),
                 column(width=8,uiOutput("ptype"))),
               fluidRow(
                 column(width=3,uiOutput("CASE")),
                 column(width=3,uiOutput("MEASURE")),
                 column(width=3,uiOutput("VARIABLE"))),
               
               fluidRow(
                 column(width=3,uiOutput("xvar")),
                 column(width=3,uiOutput("yvar")),
                 column(width=3,uiOutput("fill"))
               ),
               
               fluidRow(
                 conditionalPanel('input.plotmode=="ggplot"',
                                  column(width=12,plotOutput(outputId = "CimdoPlot",height = "450px"))),
                 conditionalPanel('input.plotmode==="plotly"',
                                  column(width=12,plotly:::plotlyOutput(outputId = "CimdoPlotly"))),
                 conditionalPanel("input.plotmode==='ggplot'",
                                  column(width=3,downloadButton('downloadPlot', 'Download Plot')))
                )),
               
        tabPanel("Data",tags$head(tags$style("tfoot{display:table-header-group;}")),
                 fluidPage(column(12,dataTableOutput('table')))
                 ),
        
        tabPanel("Measure Explanation",
                 tags$h3("The stability measures calculated by the CIMDO are:"),
                 tags$li("Common Distress within System"),
                 tags$ul(tags$li("Joint Probability of Distress (JPoD): Joint probabilities of all institutions going default"),
                          tags$li("Banking stablity Index (BSI): The expected number of institutions defaults given that at least one institution defaults")
                         ),
                 tags$li("Distress Conditioned on Subset"),
                 tags$ul(tags$li("Group Default: Conditional probabilities between banks and insurance companies"),
                         tags$li("User Defined Groups: Probability of Distress of groups defined by user")
                         ),
                 tags$li("Distress in the System Associated with a Specific Bank"),
                 tags$ul(
                         tags$li("Spillover Coefficient: Distress dependence measure that computes the probability of distress a bank conditional on others becoming distressed. This is a weighted sum of the probability of distress of bank i given the default of each other banks in the sample"),
                         tags$li("At least 1 defaults|Security Default: Prob(At least 1 institution defaults| institution  i Default)"),
                         tags$li("Exactly 1 defaults|Security Default: Prob(Exactly least 1 institution defaults| institution  i Default)"),
                         tags$li("System|Security: Prob(all other institutions going default | institution i default)"),
                         tags$li("Security|System: Prob(institution i going default | all other institutions default)")),
                 
                 tags$li("Distress Between Specific Banks"),
                 tags$ul(tags$li("row default|col default: Bivariate Conditional Probaiblity of Distress"),
                         tags$li("row default and col default: Bivariate Joint Probaiblity of Distress")
                 ),
                 
                 tags$li("Emperical PoD: Univariate Distress measured as VaR. They the marginal probabilities of distress that are input to calculate them CIMDO")
                 ),
      
      tabPanel("CIMDO Methodology",
              tags$h2("Background"), 
              tags$p("A set of banking stability measures which take account of distress dependence among the banks in a system, thereby providing a set of tools to analyze stability from complementary perspectives by allowing the measurement of"),
              tags$li("common distress of the banks in a system"),
              tags$li("distress between specific banks"),
              tags$li("distress in the system associated with a specific bank"),
              tags$br(),
              tags$p("The approach defines the banking system as a portfolio of banks and infers the system's multivariate density (BSMD) from which the proposed measures are estimated. The BSMD embeds the banks' default inter-dependence structure that captures linear and non-linear distress dependencies among the banks in the system, and its changes at different times of the economic cycle. The BSMD is recovered using the CIMDO-approach, that in the presence of restricted data, improves density specification without explicitly imposing parametric forms that, under restricted data sets, are difficult to model. Thus, the proposed measures can be constructed from a very limited set of publicly available data and can be provided for a wide range of both developing and developed countries."),
              tags$p("This methodology was dervied by ",tags$a(href="https://www.imf.org/external/pubs/ft/wp/2009/wp0904.pdf)","Segoviano and Goodhart 2009")),  
              tags$h2("Application in major financial institutions"),
              tags$p("The CIMDO framework and other market-based indicators are considered by many authorities an important, though not exclusive, tool to support the analysis of financial stability. The value added of this framework has been widely recognized by its use in various IMF global financial Stability Reviews (GFSRs), important Financial Sector assessment Programs (FSAPs), including the US in 2010, 2015, UK in 2011 and currently in 2016, Denmark, Egypt, Israel etc., and has been adopted by various Central Banks including the Riksbank, ECB, Banque de France, Mexico CNBV, Reserve Bank of India etc.)")
             
      )
    )
  ) 
))