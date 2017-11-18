
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  titlePanel("IV curves"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("files", label = "Select files", multiple = T),
      selectInput("samples", "Samples", choices = "", multiple = T),
      actionButton(inputId = "traceButton", "Trace plot"),
      hr(),
      numericInput(inputId = "capNum", value = 1e-11, min = 1e-40, max = 1, label = "Capacitance"),
      actionButton(inputId = "ivButton", "IV curve")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "panels",
        tabPanel('summaryPlot',
                 hr(),
                 plotOutput("summaryPlot")
        ),
        tabPanel('IV curve',
                 hr(),
                 plotOutput("ivPlot"),
                 textOutput("RP")
        ),
        tabPanel('Table',
                 hr(),
                 dataTableOutput("DT")
        ),
        tabPanel('Debug',
                 hr(),
                 tags$div(
                   textInput("R_input", "Enter an R Command", ''),
                   tags$button(id = "R_send", type = "button", class = "btn action-button", "Submit",
                               style = "margin-bottom: 10px;")
                 ),
                 tags$div(
                   verbatimTextOutput("debug")
                 )
        )
      )
    )
  )
))
