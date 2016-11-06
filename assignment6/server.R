library(shiny)
library(ggplot2)

source("global.R")

shinyServer(function(input, output,session) {
  
  selectedData <-  reactive({
    # Upload the selected data and reload input options to reflect the new data set
    dataSet <- data.frame(NA)
    
    if(input$origin == 'S3'){
      if (is.null(input$s3Bucket) | is.null(input$awsregion) |is.null(input$s3Object)){
        return(NULL)
      }
      uploadS3()
      dataSet <- getS3Data(input$awsregion,input$s3Bucket,input$s3Object)
      
    } else if(input$origin == 'MongoDB'){
      if (is.null(input$host) | is.null(input$collection)){
        return(NULL)
      }
      uploadMongo()
      dataSet <- getMongoDBData(input$host,input$collection,input$query,input$limit)
    }
    else {
      inFile <- input$file1
      if (is.null(inFile)){
        return(NULL)
      }
      dataSet <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                          quote=input$quote)
    }
    
    updateSelectInput(session, "feature1", choices = names(dataSet))
    updateSelectInput(session, "feature2", choices = names(dataSet))
    updateSelectInput(session, "feature3", choices = names(dataSet))
    updateSelectInput(session, "feature4", choices = names(dataSet))
    updateSelectInput(session, "feature5", choices = names(dataSet))
    updateSelectInput(session, "feature6", choices = names(dataSet))
    dataSet
  })
  
  output$s3Object <- renderUI({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$awsregion) | is.null(input$s3Bucket))
      return()
    
    # Get the all objects awailable on the passed aws reagion and S3 bucket
    objectList <- listS3Objects(input$awsregion,input$s3Bucket)
    
    # Create the drop down list for the found objects
    selectInput("s3Object", "Select S3 Object:", 
                choices  = objectList)
    
  })

  #-----------------------------------------------
  
  uploadS3 <- eventReactive(input$uploadS3, {
    input$objectList
  })

  #-----------------------------------------------
  
  output$db <- renderUI({
    if (is.null(input$host)){
      return(NULL)
    }
    host <- getMongoDBDatabase(input$host)
    selectInput("db", "Select data base:", choices <- host)
  })

  #-----------------------------------------------
  
  output$collection <- renderUI({
    if (is.null(input$host) | is.null(input$db))
      return(NULL)
    collection <- getMongoDBCollection(input$host, input$db)
    selectInput("collection", "Select collection:", choices <- collection)
  })

  #-----------------------------------------------
  
  uploadMongo <- eventReactive(input$uploadMongo, {
    input$collection
  })
  
  #-----------------------------------------------

  output$dataTable <- renderDataTable({
    
    selectedData()
  }, options = list(
    lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', '50')),
    pageLength = 10
  )
  )
  
  #-----------------------------------------------

  output$nrow <- renderText({
    nrow(selectedData())
  })

  #-----------------------------------------------

  output$feature1 <- renderText({
    selectedFeature1()
  })
  
  #-----------------------------------------------

  output$ncol <- renderText({
    ncol(selectedData())
  })
  
  #-----------------------------------------------

  output$summary <- renderPrint({
    dataSet <- selectedData()
    summary(dataSet[,input$feature1])
  })
  
  #-----------------------------------------------

  output$str <- renderPrint({
    dataSet <- selectedData()
    str(dataSet[,input$feature1])
  })
  
  #-----------------------------------------------

  output$plot <- renderPlot({
    dataSet <- selectedData()
    
    x <- na.omit(dataSet[,input$feature1])
    
    if (input$plotType == "Histogram") {
      if (is.numeric(x)){
        hist(x, 
             probability = TRUE, 
             breaks = as.numeric(input$n_breaks),
             xlab = input$feature1, 
             col = "grey",
             main = "")
        abline(v = mean(x), col = "blue", lwd = 2)
        abline(v = median(x), col = "red", lwd = 2)
        legend("topright", c("mean", "median"), col=c("blue", "red"), lwd=10)
      }
      
    } else if (input$plotType == "Box Plot") {
      boxplot(x,
              horizontal = TRUE,
              axes = FALSE,
              col="grey",
              xlab = input$feature1
      )
    } else if (input$plotType == "Dot Plot") {
      dotchart(x, cex=.7,
               xlab=input$feature1)
      
    } else if (input$plotType == "Density") {
      hist(x, 
           probability = TRUE, 
           breaks = as.numeric(input$n_breaks),
           xlab = input$feature1, main = "")
      dens <- density(x, 
                      adjust = input$bw_adjust)
      lines(dens, col = "green")
      abline(v = mean(x), col = "blue", lwd = 2)
      abline(v = median(x), col = "red", lwd = 2)
      legend("topright", c("mean", "median","density"), col=c("blue", "red","green"), lwd=10)
    }
  })
  
  #-----------------------------------------------

  output$pairPlot <- renderPlot({
    dataSet <- selectedData()
    
    if (input$fun == "normal"){
      x <- dataSet[,input$feature1]
      y <- dataSet[,input$feature2]
    } else if (input$fun == "log2"){
      x <- paste("log2(",dataSet[,input$feature1],")",sep="")
      y <- paste("log2(",dataSet[,input$feature2],")",sep="")
    } else if (input$fun == "log10"){
      x <- paste("log10(",dataSet[,input$feature1],")",sep="")
      y <- paste("log10(",dataSet[,input$feature2],")",sep="")
    } else if (input$fun == "^2"){
      x <- paste(dataSet[,input$feature1],"^2",sep="")
      y <- paste(dataSet[,input$feature2],"^2",sep="")
    } else if (input$fun == "^3"){
      x <- paste(dataSet[,input$feature1],"^3",sep="")
      y <- paste(dataSet[,input$feature2],"^3",sep="")
    }
    
    f3 <-dataSet[,input$feature3]
    qplot(x, y, data = dataSet, na.rm=TRUE) +
      stat_smooth() +
      geom_point(aes(colour = factor(f3))) 
  })
  
  #-----------------------------------------------

  output$correlations <- renderPlot({
    dataSet <- selectedData()
    
    x <- dataSet[, c(input$feature1,input$feature2,input$feature3,input$feature4,input$feature5,input$feature6)]
    panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex * abs(r))
    }
    
    pairs(x, lower.panel = panel.smooth, upper.panel = panel.cor)
  })
  
})