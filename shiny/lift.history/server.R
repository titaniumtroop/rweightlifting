library(shiny)
library(rweightlifting)
#library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)

#weightlifting.log <- load_csv_data(datadir = "../../raw-data/")
weightlifting.log <- load_csv_data(datadir = "~/.bin/rweightlifting/raw-data/")
weightlifting.log <- weightlifting.log %>%
  arrange(date) %>%
  mutate(
    exercise = factor(exercise),
    tonnage = weight * reps,
    date = as.Date(date),
    program = factor(program)
  )

start.date <- min(weightlifting.log$date, na.rm=TRUE)
today <- Sys.Date()

programs <- weightlifting.log %>%
  group_by(program) %>%
  summarize(last.date = max(date, na.rm = TRUE)) %>%
  arrange(last.date)

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

session.weights <- weightlifting.log
session.tonnage <- tonnage
session.models <- tonnage
true.max <- as.logical(no.light.day.top.sets(session.models$top.set))
session.models <- subset(session.models, true.max)

shinyServer(function(input, output, session) {
  updateSelectInput(
    session,
    inputId = "exercise",
    choices = exercises,
    selected = c("squat", "bench", "row", "press", "deadlift")
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

  output$plot.tonnage <- renderPlotly({

    temp.plot <- ggplot(
      data = session.tonnage(),
#      data = tonnage,
        aes(
          x = date,
          y = tonnage,
          fill = exercise,
          group = top.set,
          text = paste("date: ", date, "<br>exercise: ", exercise, "<br>tonnage: ", tonnage, "<br>top set: ", top.set)
        ),
        width = 2.5
      ) +
      geom_bar(
        position = "stack",
        stat = "identity"
      ) +
      scale_fill_brewer(name = "Tonnage", palette = "Paired") +
        labs(
          title = "Tonnage",
          x = "", y = ""
        )

    (ggplotly(temp.plot, tooltip = "text") %>% layout(legend = list(traceorder = "reversed")))
 })

  output$max.tonnages <- renderPlotly({
    temp <- program.tonnage()
    temp.plot <- ggplot(
      temp,
      aes(
        x = factor(exercise, rev(exercise)),
        y = max.tonnage,
        fill = factor(program, rev(program)),
        text = paste("program: ", factor(program, rev(program)), "<br>exercise: ", factor(exercise, rev(exercise)), "<br>max tonnage: ", max.tonnage)
      )
      ) +
      scale_fill_brewer("Program", palette="Paired", guide = guide_legend(reverse=FALSE)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Max Tonnage by Program", x = "", y = "") +
      coord_flip()

      (ggplotly(temp.plot, tooltip = "text") %>% layout(legend = list(traceorder = "reversed")))
  })

  output$plot.full <- renderPlotly({
    temp.plot <- ggplot(
        data = session.weights(),
        aes(date, weight, color = exercise, size = reps)
      ) +
      scale_color_brewer(palette="Paired") +
      facet_wrap( ~ exercise) +
      geom_point(
        aes(
          date,
          weight,
          color = variant,
          text = paste("date: ", date, "<br>exercise: ", exercise, "<br>weight: ", weight, "<br>reps: ", reps, "<br>variant: ", variant)
        ),
        alpha=0.5
      ) +
      scale_radius(
        range = c(1, 6),
        breaks=c(3,6,9,12)
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
      theme(
        panel.background = element_rect(fill = "gray95")
      ) +
      geom_vline(aes(xintercept = as.numeric(Sys.Date())), color=alpha("green", 0.5), linetype="dashed")+
      labs(title = "Sets x Reps", x = "", y = "")

    (ggplotly(temp.plot, tooltip = "text") %>% layout(legend = list(traceorder = "reversed")))
  })

  # Plot.ly bug transposes negative numbers to positive here
  # see https://github.com/ropensci/plotly/issues/560
  output$plot.maxes <- renderPlot({
    ggplot(program.maxes()) +
      scale_fill_brewer(
        "Program",
        palette="Paired",
        guide = guide_legend(reverse=TRUE)
      ) +
      geom_bar(
        aes(
          factor(exercise, rev(exercise)),
          diff.percent,
          fill = factor(program, levels=rev(program)),
          group = factor(program, levels=rev(program))
        ),
        stat = "identity",
        position = "dodge") +
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
