library(shiny)
library(plotly)
library(ggridges)
library(DT)

shinyUI(fluidPage(
  titlePanel("RWeightlifting — Past Performance"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
      )),
      fluidRow(sliderInput(
        "plot.width",
        "Plot Width",
        100,
        2000,
        1200,
        step = 50,
        sep = ""
      )),
      fluidRow(sliderInput(
        "plot.height",
        "Plot Height",
        100,
        2000,
        900,
        step = 50,
        sep = ""
      ))
    ),
    mainPanel(
      # Suppress transient faceting error that appears before page load
      # tags$style(
      #   type="text/css",
      #   ".shiny-output-error { visibility: hidden; }",
      #   ".shiny-output-error:before { visibility: hidden; }"
      # ),
      width = 9,
      tabsetPanel(
        tabPanel("Sets vs. Reps", plotlyOutput(outputId = "plot.full", width = "100%", height = "100%")),
        tabPanel("Lift PRs", plotlyOutput(outputId = "plot.maxes", width = "100%", height = "100%")),
        tabPanel(
          "Rep PRs", 
          fluidRow(sliderInput(
            "minweight",
            "Minimum Weight",
            0,
            500,
            100,
            step = 5,
            sep = ""
          )),
          plotlyOutput(outputId = "plot.max.reps", width = "100%", height = "100%")
        ),
        tabPanel("Daily Tonnage", plotlyOutput(outputId = "plot.tonnage", width = "100%", height = "100%")),
        tabPanel("Weekly Tonnages", plotlyOutput(outputId = "weekly.tonnages", width = "100%", height = "100%")),
        tabPanel("Tonnage PRs", plotlyOutput(outputId = "max.tonnages", width = "100%", height = "100%")),
        tabPanel("Ridgeline", plotOutput(outputId = "plot.joy")),
        tabPanel("Summary Table", DT::dataTableOutput("textout")),
        tabPanel("Lift PR Table", DT::dataTableOutput("program.maxes")),
        tabPanel("Lift PR Table", DT::dataTableOutput("tonnage.maxes"))
      )
    )
  )
))
