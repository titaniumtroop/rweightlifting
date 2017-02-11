library(shiny)
library(rweightlifting)
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)

weightlifting.log <- load_csv_data(datadir = "../../raw-data/")
weightlifting.log <- weightlifting.log %>%
  arrange(date) %>%
#  mutate(program = factor(program, levels = weightlifting.log$program, ordered = TRUE)) %>%
  mutate(
    exercise = factor(exercise),
    tonnage = weight * reps,
    date = as.Date(date)
  )
session.weights <- weightlifting.log

start.date <- min(weightlifting.log$date, na.rm=TRUE)
today <- Sys.Date()

programs <- weightlifting.log %>%
  group_by(program) %>%
  summarize(last.date = max(date)) %>%
  arrange(last.date) %>%
  mutate(program = factor(program, levels = unique(tonnage$program), ordered = TRUE))

weightlifting.log <- weightlifting.log %>%
  mutate(program = factor(program, levels = programs$program, ordered = TRUE))

tonnage <- weightlifting.log %>%
  group_by(program, date, exercise) %>%
  summarize(
    top.set = max(weight),
    tonnage = sum(tonnage)
   ) %>%
  arrange(date, exercise, desc(top.set))

exercises <- unique(weightlifting.log$exercise)

session.tonnage <- tonnage
session.models <- tonnage
true.max <- as.logical(no.light.day.top.sets(session.models$top.set))
session.models <- subset(session.models, true.max)

shinyServer(function(input, output, session) {
  updateSelectInput(
    session,
    inputId = "exercise",
    choices = exercises,
    selected = c("squat", "bench", "row", "press", "deadlift"),
  )

  updateSelectInput(
    session,
    inputId = "program",
    choices = programs$program,
    selected = programs$program
  )

  updateDateRangeInput(
    session,
    inputId = "date",
    start = start.date,
    end = today
  )

  output$plot.tonnage <- renderPlot({

    temp.plot <- ggplot() +
      geom_bar(data = session.tonnage(),
               aes(date, tonnage, fill = exercise),
               width = 2.5,
               stat = "identity"
              ) +
    scale_fill_brewer(name = "Tonnage", palette = "Paired") +
      labs(
        title = "Tonnage",
        x = "", y = ""
      )

    temp.plot
 })

  output$max.tonnages <- renderPlot({
    temp <- program.tonnage()
    ggplot(temp) +
      scale_fill_brewer("Program", palette="Paired", guide = guide_legend(reverse=TRUE)) +
      geom_bar(aes(factor(exercise, rev(exercise)), max.tonnage, fill = factor(program, rev(program))), stat = "identity", position = "dodge") +
      labs(title = "Max Tonnage by Program", x = "", y = "") +
      coord_flip()
  })

  output$plot.full <- renderPlot({
    ggplot(
        data = session.weights(),
        aes(date, weight, color = exercise, size = reps)
      ) +
      scale_color_brewer(palette="Paired") +
      facet_wrap( ~ exercise) +
      geom_point(
        aes(date, weight, color=exercise),
        alpha=0.85
      ) +
      scale_radius(
        range = c(0.5, 4),
        breaks=c(3,6,9,12,15)
      ) +
      scale_y_continuous(
        position="right",
        minor_breaks=function(x){
          seq(0, ceiling(max(x)), 25)
        }) +
      scale_x_date(
        date_breaks = "3 month", date_labels = "%b %y",
        date_minor_breaks = "1 month"
      ) +
      geom_vline(aes(xintercept = as.numeric(Sys.Date())), color=alpha("green", 0.5), linetype="dashed")+
      labs(title = "Sets x Reps", x = "", y = "")

  })
  
  output$plot.maxes <- renderPlot({
    ggplot(program.maxes()) +
      scale_fill_brewer("Program", palette="Paired", guide = guide_legend(reverse=TRUE)) +
      geom_bar(aes(factor(exercise, rev(exercise)), diff.percent, fill = factor(program, levels=rev(program)), group = factor(program, levels=rev(program))), stat = "identity", position = "dodge") +
      labs(title = "Max Weight", subtitle = "Percent Improvement over Previous Program", x = "", y = "") +
      coord_flip()
  })

  session.weights <- reactive({
    weightlifting.log %>%
      mutate(exercise = factor(exercise, levels = exercisef())) %>%
      filter(date >= as.Date(input$date[1], format = "%Y-%m-%d") & date <= as.Date(input$date[2], format = "%Y-%m-%d")) %>%
      filter(program %in% input$program) %>%
      filter(exercise %in% input$exercise)
  })

  session.tonnage <- reactive({
    temp <- tonnage %>%
      mutate(exercise = factor(exercise, levels = exercisef())) %>%
      filter(
        date >= as.Date(input$date[1], format = "%Y-%m-%d") &
        date <= as.Date(input$date[2], format = "%Y-%m-%d")
      ) %>%
      filter(program %in% input$program) %>%
      filter(exercise %in% input$exercise)
    temp
  })

  program.tonnage <- reactive({
    session.tonnage() %>%
      mutate(exercise = factor(exercise, levels = exercisef())) %>%
      group_by(program, exercise) %>%
      summarize(max.tonnage = max(tonnage))
  })

  program.maxes <- reactive({
    # Look at Contracts Notes app to get correct piped logic
#    temp <- weightlifting.log %>%
    temp <- session.weights() %>%
      mutate(exercise = factor(exercise, levels = exercisef())) %>%
      group_by(program, exercise) %>%
      summarize(
        max.lift = max(weight),
        last.date = max(date)
      ) %>%
      arrange(exercise)
    temp <- temp %>%
      group_by(exercise) %>%
      mutate(prev.max = lag(max.lift)) %>%
      mutate(diff = max.lift - prev.max) %>%
      mutate(diff.percent = diff / max.lift * 100)

    temp
  })
  
  exercisef <- reactive({
    temp <- factor(input$exercise, ordered = TRUE)
    temp
  })
  
})
