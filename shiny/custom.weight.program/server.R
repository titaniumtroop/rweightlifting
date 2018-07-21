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
  #  filter(! exercisef %in% c("tricep extension", "bicep curl", "pullup", "dips")) %>%
  mutate(
    exercise = factor(exercise),
    tonnage = weight * reps,
    date = as.Date(date)
#    date = as.POSIXct(paste(date, " 18:00:00", sep=""), format = "%Y-%m-%d", tz = "America/Chicago")
  )
session.weights <- weightlifting.log

start.date <- min(weightlifting.log$date, na.rm=TRUE)
today <- Sys.Date()

programs <- unique(weightlifting.log$program)
exercises <- unique(weightlifting.log$exercise)

tonnage <- weightlifting.log %>%
  group_by(program, date, exercise) %>%
  summarize(
    top.set = max(weight),
    tonnage = sum(tonnage)
   ) %>%
  arrange(date, exercise, desc(top.set))

session.tonnage <- tonnage
session.models <- tonnage
true.max <- as.logical(no.light.day.top.sets(session.models$top.set))
session.models <- subset(session.models, true.max)

planned.table <- table(c(0,0))

shinyServer(function(input, output, session) {
  updateSelectInput(
    session,
    inputId = "exercise",
    choices = exercises,
    selected = c("bench")
  )

  updateSelectInput(
    session,
    inputId = "program",
    choices = programs,
    selected = programs
  )

  updateDateRangeInput(
    session,
    inputId = "date",
    start = start.date,
    end = today
  )

  output$plot.tonnage <- renderPlot({
    cols <- c("Guess"="red","Predicted"="green", "Observed" = "blue", "Planned" = "orange")

    temp.plot <- ggplot() +
      geom_bar(data = session.tonnage(),
               aes(date, tonnage, fill = "Observed"),
               width = 2.5,
               stat = "identity"
              ) +
      geom_bar(
        data = planned.tonnage(),
        aes(pDate, tonnage, fill = "Planned"),
        width = 2.5,
        stat = "identity"
      ) +
    scale_fill_manual(name = "Tonnage", values = cols) +
      labs(
        title = "Tonnage",
        x = "", y = ""
      )

    temp.plot
 })

  output$plot.models <- renderPlot({
    session.models()
  })

  output$planned.table <- renderTable({
    planned.table()
  })

  output$planned.tonnage <- renderTable({
    planned.tonnage()
  })

  session.tonnage <- reactive({
    temp <- 
      weightlifting.log %>%
      filter(
        date >= as.Date(input$date[1], format = "%Y-%m-%d") &
        date <= as.Date(input$date[2], format = "%Y-%m-%d")
      ) %>%
      filter(program %in% input$program) %>%
      filter(reps <= input$cutoff_reps) %>%
      filter(exercise %in% input$exercise) %>%
      group_by(program, date, exercise) %>%
      summarize(
        top.set = max(weight),
        tonnage = sum(tonnage)
      ) %>%
      arrange(date, exercise, desc(top.set))
    temp
  })

  session.models <- reactive({
    # Filter cutoff % and reps
    temp.max <- max(session.tonnage()$top.set, na.rm = TRUE)

    temp.df <- session.tonnage() %>%
      filter(top.set >= input$cutoff_percent / 100 * temp.max)

    # Now handled with Shiny
    # Remove light day sets (<85% previous top set for exercise), primarily for squat
    true.max <- as.logical(no.light.day.top.sets(temp.df$top.set))
    temp.df <- subset(temp.df, true.max)
    
    

    plots <- (predicted.vs.planned(
      top.set.history = temp.df,
      cycle.workouts = input$cycles,
      increase.after = input$days,
      increase.weight = input$increment,
      deload.factor = input$deload / 100
    ))
    plots
  })

  planned.table <- reactive({
    temp.df <- tonnage %>%
      filter(program %in% input$program) %>%
      filter(exercise %in% input$exercise)

    # Remove light day sets (<85% previous top set for exercise), primarily for squat
    true.max <- as.logical(no.light.day.top.sets(temp.df$top.set))
    temp.df <- subset(temp.df, true.max)

    plot.table <- planned(
      top.set.history = temp.df,
      cycle.workouts = input$cycles,
      increase.after = input$days,
      increase.weight = input$increment,
      deload.factor = input$deload / 100,
      ramp = input$ramp / 100,
      sets = input$sets,
      reps = input$reps,
      plate = input$plate
    )
    plot.table %>% spread(workout, weight)
  })

  # Get planned table, calculate tonnage each day, add to tonnage table with different color
  planned.tonnage <- reactive({
    planned.table() %>%
      gather(day, weight) %>%
      filter(day != "set" & day != "reps") %>%
      mutate(num = as.numeric(day) - min(as.numeric(day), na.rm = TRUE)) %>%
      mutate(pDate = as.Date(Sys.Date()) + as.numeric(num) * floor(7 / input$days)) %>% # how many days per week?
      group_by(pDate) %>%
      summarize(tonnage = sum(weight * input$reps))
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste(input$exercise, '.csv', sep='') },
    content = function(file) {
      write.csv(planned.table(), file)
    }
  )

})
