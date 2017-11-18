library(data.table)
library(ggplot2)
library(shiny)

source("IVCR.R")

shinyServer(function(input, output, session) {

  # Select files then populate the sample selector
  observeEvent(input$files, {
    files <<- input$files
    names <- files$name
    updateSelectInput(session, "samples", "Samples",
                      choices = names)
  })
  
  # Read selected file and draw plot
  observeEvent(input$traceButton, {
    seldata <<- readfiles(input$files)
    #selsample <<- input$samples
    #path <- files[files$name == selsample, "datapath"]
    #seldata <<- fread(path)
    #setnames(seldata, "V1", "y")
    #seldata[, x := 1:2000]
    #seldata <<- seldata[, sample := selsample]
  })
  
  output$DT <- renderDataTable({
    input$traceButton
    isolate({
      if(input$traceButton > 0){
        return(seldata)
      }
    })
  })
  
  output$summaryPlot <- renderPlot({
    input$traceButton
    isolate({
      if(input$traceButton > 0){
        p <- makeTracePlot(seldata)
        p
        }
    })
  })
  
  # IV curves
  observeEvent({
    input$ivButton
    },{
    dat <<- seldata[x > 1000 & x <= 1500]
    dat[, newx := seq(-100, 100, length.out = 500)]
    dat[, newy := y/input$capNum]
    
    # Fit a 3degree polynomial
    fitobj <- lm(newy ~ newx + I(newx^2) + I(newx^3), data = dat)
    sry <- summary(fitobj)
    
    coefs <- sry$coefficients[,1]
    dat[, i := coefs[1]]
    dat[, a := coefs[2]]
    dat[, b := coefs[3]]
    dat[, c := coefs[4]]
    icepts <- polyroot(coefs)
    dat[, rev_p := find0(icepts)]
    dat[, fitted := i + newx*a + newx^2*b + newx^3*c]
  })
  output$RP <- renderText({
    paste0("Reverse potential: ", round(unique(dat$rev_p), 4))
  })
  
  output$ivPlot <- renderPlot({
    input$ivButton
    isolate({
      if(input$ivButton > 0){
        p <- makeIVPlot(dat)
        p
      }
    })
  })
  
  ## DEBUG
  output$debug <- renderPrint({
    R_send <- input$R_send
    if (input$R_send == 0) {
      return( invisible(NULL) )
    }
    isolate({
      code <- input$R_input
      result <- eval( parse( text=code ) )
      return(result)
    })
  })
})
