#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("RWeightlifting - Custom Program Builder"),
  sidebarLayout(
    sidebarPanel(
      h3("Performance"),
      fluidRow(dateRangeInput("date", "Date Range", start = "2016-01-01")),
      fluidRow(selectInput(
        "program",
        "Program name",
        choices = c("SL 5x5", "Madcow 5x5", "Texas Method"),
        multiple = TRUE,
        selectize = TRUE)
      ),
      fluidRow(selectInput(
        "exercise",
        "Exercise",
        choices = c("bench", "squat", "deadlift"),
        selected = "squat"
        )
      ),
      h3("Custom Programming"),
      fluidRow(sliderInput("deload", "Deload\n(% of max work set)", 0, 100, 85, step= 2.5)),
      fluidRow(sliderInput("increment", "Increment weight", 0, 20, 5, step= 2.5)),
      fluidRow(sliderInput("cycles", "No. of cycles", 0, 50, 24, step = 1)),
      fluidRow(sliderInput("days", "Lift days per cycle", 1, 5, 2, step = 1)),
      fluidRow(sliderInput("sets", "No. of sets", 1, 10, 5, step = 1)),
      fluidRow(sliderInput("reps", "No. of repetitions", 1, 25, 5, step = 1)),
      fluidRow(sliderInput("plate", "Smallest plate", 1.25, 5, 2.5, step = 1.25)),
      fluidRow(sliderInput("ramp", "Ramp % (set-to-set increase)", 0, 20, 12.5, step = 0.5))

    ),
    mainPanel(
      # Suppress transient faceting error that appears before page load
      # tags$style(
      #   type="text/css",
      #   ".shiny-output-error { visibility: hidden; }",
      #   ".shiny-output-error:before { visibility: hidden; }"
      # ),
      fluidRow(
        plotOutput(outputId = "plot.tonnage")
      ),
      fluidRow(
        plotOutput(outputId = "plot.models")
      ),
      fluidRow(
        tableOutput(outputId = "planned.table")
      ),
      downloadButton('downloadData', 'Download')#,
      # fluidRow(
      #   tableOutput(outputId = "planned.tonnage")
      # )
    )
  )
))
