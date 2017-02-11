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
  titlePanel("RWeightlifting — Past Performance"),
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
        selected = "squat",
        multiple = TRUE,
        selectize = TRUE
      ))
    ),
    mainPanel(
      # Suppress transient faceting error that appears before page load
      # tags$style(
      #   type="text/css",
      #   ".shiny-output-error { visibility: hidden; }",
      #   ".shiny-output-error:before { visibility: hidden; }"
      # ),
      tabsetPanel(
        tabPanel("Tonnage", plotOutput(outputId = "plot.tonnage")), 
        tabPanel("Sets vs. Reps", plotOutput(outputId = "plot.full")), 
        tabPanel("Max Tonnages", plotOutput(outputId = "max.tonnages")), 
        tabPanel("Max Weight by Program", plotOutput(outputId = "plot.maxes"))
      )
      # fluidRow(
      #   plotOutput(outputId = "plot.tonnage")
      # ),
      # fluidRow(
      #   plotOutput(outputId = "plot.full")
      # ),
      # fluidRow(
      #   plotOutput(outputId = "max.tonnages")
      # ),
      # fluidRow(
      #   plotOutput(outputId = "plot.maxes")
      # )
    )
  )
))
