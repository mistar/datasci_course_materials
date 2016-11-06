library(shiny)
source("global.R")

# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Exploratory Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(

      #-----------------------------------------------

      conditionalPanel(
        condition = "input.datatabs == 'Data'",  
        radioButtons('origin', 'Data Origin',
                     c(S3 = 'S3',
                       MongoDB = 'MongoDB',
                       PC ='PC'),
                     inline=TRUE
        )
      ), #conditionalPanel
      
      #-----------------------------------------------
      
      conditionalPanel(
        condition = "input.datatabs == 'Data' && input.origin == 'S3'",   
        
        textInput("awsregion", label = h5("AWS region:"),  value = ""),
        textInput("s3Bucket", label = h5("S3 Bucket:"),  value = "")
      ), #conditionalPanel
      
      #-----------------------------------------------
      
      conditionalPanel(
        condition = "input.datatabs == 'Data' && input.origin == 'S3' 
          && input.awsregion != '' && input.s3Bucket != ''", 
        uiOutput("s3Object"),
        actionButton("uploadS3", "Upload data")
        
      ), #conditionalPanel
      
      #-----------------------------------------------
      
      conditionalPanel(
        condition = "input.datatabs == 'Data' && input.origin == 'MongoDB'",
        textInput("host", label = h5("Host:"),  value = "")
      ), #conditionalPanel
      
      #-----------------------------------------------
      
      conditionalPanel(
        condition = "input.datatabs == 'Data' && input.origin == 'MongoDB' 
          && input.host != ''",   
        uiOutput("db")
      ), #conditionalPanel
      
      #-----------------------------------------------
      
      conditionalPanel(
        condition = "input.datatabs == 'Data' && input.origin == 'MongoDB' 
          && input.host != '' && input.db != ''",   
        uiOutput("collection"),
        textInput("query", label = h5("Query:"),  value = ""),
        textInput("limit", label = h5("Limit:"),  value = "100"),
        actionButton("uploadMongo", "Upload data")
      ), #conditionalPanel

      #-----------------------------------------------
      
      conditionalPanel(
        condition = "input.datatabs == 'Data' && input.origin == 'PC'",    
        
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')
        ),
        
        tags$hr(),
        
        checkboxInput('header', 'Header', TRUE),
        
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')
      ), #conditionalPanel
      
      #-----------------------------------------------
      
      conditionalPanel(
        condition = "input.datatabs != 'Data'",   
        
        selectInput('feature1', 
                    label = 'Base feature:', 
                    choices = names(dataSet)
        )
      ), #conditionalPanel
      
      #-----------------------------------------------

      conditionalPanel(
        condition = "input.datatabs == 'Pairs' | input.datatabs == 'Correlations'",   
        
        selectInput('feature2', 
                    label = 'Second feature:', 
                    choices = names(dataSet),
                    selected = names(dataSet)[1]),
        selectInput('feature3', 
                    label = 'Third feature:', 
                    choices = names(dataSet),
                    selected = names(dataSet)[3])
      ), #conditionalPanel
      
      #-----------------------------------------------

      conditionalPanel(
        condition = "input.datatabs == 'Correlations'",   
        
        selectInput('feature4', 
                    label = 'Forth feature:', 
                    choices = names(dataSet),
                    selected = names(dataSet)[4]),
        selectInput('feature5', 
                    label = 'Fifth feature:', 
                    choices = names(dataSet),
                    selected = names(dataSet)[5]),
        selectInput('feature6', 
                    label = 'Sixth feature:', 
                    choices = names(dataSet),
                    selected = names(dataSet)[6])
      ), #conditionalPanel
      
      #-----------------------------------------------

      conditionalPanel(
        condition = "input.datatabs == 'Pairs'",   
        
        selectInput('fun', 
                    label = 'Function:', 
                    c("normal", "log2","log10","^2","^3"))
      ), #conditionalPanel
      
      #-----------------------------------------------

      conditionalPanel(
        condition = "input.datatabs == 'Feature'",   
        
        selectInput("plotType", 
                    label = "Type of the plot:",
                    choices = c("Histogram", "Box Plot",
                                "Dot Plot","Density")
        )
      ), #conditionalPanel
      
      #-----------------------------------------------
      
      conditionalPanel(
        condition = ("(input.plotType == 'Histogram' | input.plotType == 'Density' ) & input.datatabs == 'Feature'"),
        
        sliderInput("n_breaks",
                    label =  "Bins:",
                    min = 1,
                    max = 50,
                    value = 20)
      ), #conditionalPanel
      
      #-----------------------------------------------
      conditionalPanel(
        condition = ("input.plotType == 'Density' & input.datatabs == 'Feature'"),
        
        sliderInput("bw_adjust", 
                    label = "Bandwidth adjustment:",
                    min = 1, 
                    max = 20, 
                    value = 5, 
                    step = 1)
      ) #conditionalPanel
    ), # sidebarPanel
    
    #--------------------------------------------------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------------------------------------------------
    
    # Main pannel lay-out
    mainPanel(
      tabsetPanel(id = "datatabs",
                  tabPanel("Data", 
                           fluidRow(
                             column(2,
                                    h5("Number of Rows:"),
                                    textOutput("nrow")
                             ),
                             column(2,
                                    h5("Number of Columns:"),
                                    textOutput("ncol")
                             )
                           ),                           
                           h5("Data:"),
                           dataTableOutput('dataTable')
                  ), #tabPanel
                  
                  #-----------------------------------------------

                  tabPanel("Feature",
                           fluidRow(
                             column(5,
                                    h5("Summary:"),
                                    textOutput("summary")
                             ),
                             column(7,
                                    h5("Str:"),
                                    textOutput("str")
                             )
                           ),
                           plotOutput("plot")
                  ), #tabPanel
                  
                  #-----------------------------------------------
                  
                  tabPanel("Pairs", 
                           plotOutput("pairPlot")
                  ), #tabPanel
                  
                  #-----------------------------------------------
                  
                  tabPanel("Correlations", 
                           plotOutput("correlations")
                  ) #tabPanel

                  #-----------------------------------------------

      ) #tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
)) # shinyUI