#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(DT)
library(shinyalert)


header = dashboardPage(
  dashboardHeader(title = "RANDOMIZE"),
  sidebar =  dashboardSidebar(
    sidebarMenu( id = "items",
                 menuItem("Introduction", tabName = "introduction", icon = icon("fa fa-info-circle")),
                 menuItem("Tutorial", tabName = "help", icon = icon("fa fa-info-circle")),
                 menuItem("Analysis", tabName = "analysis", icon = icon("fa fa-info-circle")),
                 
                 menuItem("Downloads", tabName = "Downloads", icon = icon("fa fa-info-circle"),
                          list(
                            downloadButton('randdownload', 'Randomized Data'),
                            br(),
                            br(),
                            downloadButton("finaldownload",'Final Design'),
                            
                            br(),
                            br(),
                            
                            downloadButton("plotdownload",'Plot')
                          )
                 )
    )),
  
  body = dashboardBody(
    
    conditionalPanel(
      condition = "input.items == 'introduction'",
      
      h2(strong("RANDOMIZE", style = "color:gray")),
      h4(p(" High-throughput DNA methylation arrays are susceptible to bias 
          facilitated by batch effects and other technical noise that 
          can alter DNA methylation level estimates. RANDOMIZE is a 
          user-friendly web application that provides an interactive 
          and flexible graphical user interface (GUI) to randomize relevant 
          metadata. Using this tool will minimize chip and position mediated
          batch effects in microarray studies for an increased validity in 
          inferences from your methylation data. The tool is very helpful 
          for biologist to perform randomization of test samples 
          and insert controls in the data")),
      br(),
      br()
    ),
    
    conditionalPanel(
      condition = "input.items == 'help'",
      h2(strong("Tutorial", style = "color:gray")),
      h4(p("Sample data and tutorial are available to download.")
      ),
      
      br(),
      
      downloadButton("sampledata",'Sample Data'),
      
      downloadButton("TutorialDownl",'Tutorial'),
    ),
    
    conditionalPanel(
      
      condition = "input.items == 'analysis'",
      fluidRow(
        tabBox(
          title = "Randomization",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", width = 8, height = "530px",
          tabPanel("Input Data", DT::dataTableOutput("datafile")),
          tabPanel("Randomized Data", DT::dataTableOutput("randomdata")),
          tabPanel("Final Design", DT::dataTableOutput('finaldata')),
          tabPanel("Plot", plotOutput("PlotSummary"))
        ),
        box(
          br(),
          title = "Randomization", width = 4, solidHeader = TRUE, status = "primary",
          h5("Upload Data Files",style="bold"),
          fileInput("InputFile", "Choose CSV file",
                    multiple = "TRUE",
                    accept=c('text/csv','text/comma-separated-values,text/plain',
                             '.csv','.cel','.TXT','.txt')),
          br(),
          checkboxInput("Controls", "Insert Controls", value = FALSE),
          
          br(),
          actionButton("Submit", label = "Submit", 
                       icon("floppy-o"),style="color: white ;
                   background-color: #008B8B;border-color: rgb(51, 153, 255)"),
          br(),
          br(),
          
          br(),)),
      fluidRow(
        uiOutput("ControlLOC"),
        useShinyalert(),
        uiOutput("Plots"),
        uiOutput("PlotLabels"),
        uiOutput('Display_final_design'),
        
        br(),
        
      ),
      fluidRow(
        column(6,div(style = "height:200px", "")))
      
    )
  )
)
  

