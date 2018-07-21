library(shiny)
library(rweightlifting)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(DT)
library(ggridges)

ifelse(
  dir.exists("/data/fitness/Weightlifting/"),
  data.dir <- "/data/fitness/Weightlifting/",
  data.dir <- "/Users/nathan/Dropbox (Personal)/Nathan Home/Documents/Personal/Health/Fitness/Data/Weightlifting/"
)

weightlifting.log <- load_csv_data(datadir = data.dir)
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
  mutate(program = factor(program, levels = unique(programs$program), ordered = TRUE))

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
    selected = c("squat", "deadlift", "bench", "press")
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

    (ggplotly(temp.plot, tooltip = "text", width = input$plot.width, height = input$plot.height) %>% layout(legend = list(traceorder = "reversed")))
 })

  output$weekly.tonnages <- renderPlotly({
    temp.plot <- ggplot(
      data = weekly.tonnages(),
      #data = weekly.tonnages,
      aes(
        x = week,
        y = tonnage,
        fill = exercise,
        group = top.set,
        text = paste("week: ", week, "<br>week of: ", weekof, "<br>exercise: ", exercise, "<br>tonnage: ", tonnage, "<br>top set: ", top.set)
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

    (ggplotly(temp.plot, tooltip = "text", width = input$plot.width, height = input$plot.height) %>% layout(legend = list(traceorder = "reversed")))
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
      labs(title = "Sets vs. Reps", x = "", y = "")

    (ggplotly(temp.plot, tooltip = "text", width = input$plot.width, height = input$plot.height) %>% layout(legend = list(traceorder = "reversed")))
  })

  output$max.tonnages <- renderPlotly({
    temp <- program.tonnage()
    temp.plot <- ggplot(
      temp,
      aes(
        x = factor(exercise, rev(unique(exercise))),
        y = max.tonnage,
        fill = factor(program, rev(unique(program))),
        text = paste(
          "program: ", factor(program, rev(unique(program))),
          "<br>exercise: ", factor(exercise, rev(unique(exercise))),
          "<br>max tonnage: ", max.tonnage,
          "<br>most recent date: ", date
        )
      )
    ) +
      scale_fill_brewer("Program", palette="Paired", guide = guide_legend(reverse=FALSE)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Max Tonnage by Program", x = "", y = "") +
      coord_flip() +
      theme(
        legend.text = element_text(size = 7)
      )

    (ggplotly(temp.plot, tooltip = "text", width = input$plot.width, height = input$plot.height) %>% layout(legend = list(traceorder = "reversed")))
  })

  output$program.maxes <- DT::renderDataTable({
    program.maxes()
  })
  
  output$tonnage.maxes <- DT::renderDataTable({
    program.tonnage()
  })

    output$plot.maxes <- renderPlotly({
    temp <- program.maxes()
    temp.plot <- ggplot(
      temp,
      aes(
        x = factor(exercise, rev(unique(exercise))),
        y = max.lift,
        fill = factor(program, levels=rev(unique(program))),
        text = paste("program: ", factor(program, rev(unique(program))),
                     "<br>exercise: ", factor(exercise, rev(unique(exercise))),
                     "<br>max lift: ", max.lift #,
                    # "<br>percent diff from previous: ", diff.percent
                    )
      )
    ) +
      scale_fill_brewer("Program", palette="Paired", guide = guide_legend(reverse=FALSE)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Max Weight by Program", x = "", y = "") +
      coord_flip() +
      theme(
        legend.text = element_text(size = 7)
      )

    (ggplotly(temp.plot, tooltip = "text", width = input$plot.width, height = input$plot.height) %>% layout(legend = list(traceorder = "reversed")))
    #temp.plot
  })

  output$plot.max.reps <- renderPlotly({
    temp.plot <- ggplot(
      #temp.rep.max,
      rep.maxes(),
      aes(
        weight,
        max.reps,
        size = count,
        fill = factor(program, rev(unique(program))),
        text = paste(
          "program: ", factor(program, rev(unique(program))),
          "<br>exercise: ", factor(exercise, rev(unique(exercise))),
          "<br>max reps: ", max.reps,
          "<br>weight: ", weight,
          "<br>no. of times: ", count,
          "<br>most recent date: ", date
        )
      )
    ) +
    facet_wrap(~ exercise) +
      scale_fill_brewer("Program", palette="Paired") +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Rep PRs", x = "", y = "") +
      coord_flip() +
      theme(
        legend.text = element_text(size = 7)
      )

    (ggplotly(temp.plot, tooltip = "text", width = input$plot.width, height = input$plot.height) %>% layout(legend = list(traceorder = "reversed")))
    #temp.plot
  })

  output$plot.joy <- renderPlot({
    ggplot(
      # weightlifting.log,
      data = session.weights(),
      aes(x = weight, y = exercise, fill = ..y..)
    ) +
      scale_fill_gradientn(
        # MotiFit colors don't look great
        # colors = c("#4ea7d0", "#4ea7d0", "#0ab863", "#d1be22", "#d17d09", "#d1362e"),
        colors = c("royalblue", "royalblue", "green", "yellow", "orange", "red")
        #   breaks = breaks
      ) +
      geom_density_ridges(na.rm = TRUE, col = "grey70", scale = 2, bandwidth = 5) +
      theme_ridges(font_size = 10) +
      facet_wrap(~ program) +
      labs(x = "", y = "") +
      theme(
        legend.position = "none"
      )

  })

  # output$textout <- renderTable({
  #   session.weights()
  # })

  output$textout <- DT::renderDataTable({
    exercise.stats <- session.weights() %>%
      group_by(exercise) %>%
      summarize(
        total.sets = n(),
        total.reps = sum(reps),
        avg.reps = round(mean(reps), 1),
        avg.weight = round(mean(weight), 1),
        median.weight = median(weight),
        sd.weight = round(sd(weight), 1)
      ) %>%
      arrange(desc(total.sets))

    exercise.stats
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
    # tonnage %>%
      mutate(exercise = factor(exercise, levels = exercisef())) %>%
      group_by(program, exercise) %>%
      summarize(max.tonnage = max(tonnage)) %>%
      left_join(
        # tonnage,
       session.tonnage(),
        by = c("program" = "program", "exercise" = "exercise", "max.tonnage" = "tonnage")
      ) %>%
      distinct() %>%
      group_by(program, exercise, max.tonnage) %>%
      summarise(
        date = max(date)
      )
  })

  program.maxes <- reactive({
    #temp.program.maxes <- weightlifting.log %>%
    temp.program.maxes <- session.weights() %>%
      #mutate(exercise = factor(exercise, levels = exercisef())) %>%
      group_by(program, exercise) %>%
      summarize(
        max.lift = max(weight),
        last.date = max(date)
      ) %>%
      arrange(exercise)
    temp.program.maxes <- temp.program.maxes %>%
      group_by(exercise) %>%
      mutate(prev.max = lag(max.lift, default = 0)) %>%
      mutate(diff = max.lift - prev.max) %>%
      mutate(diff.percent = ifelse(is.na(diff), diff, paste(as.character(round(diff / max.lift * 100), 1), "%", sep=""))) #%>%
#      mutate(diff.percent = as.numeric(diff.percent))

    temp.program.maxes
  })

  rep.maxes <- reactive({
    # temp.rep.max <- weightlifting.log %>%
    temp.rep.max <- session.weights() %>%
      #mutate(exercise = factor(exercise, levels = exercisef())) %>%
      filter(weight >= input$minweight) %>%
      # filter(weight >= 80) %>%
      group_by(exercise, weight) %>%
      summarize(
        max.reps = max(reps)
      ) %>%
      ungroup() %>%
      left_join(
        session.weights(),
        # weightlifting.log,
        by = c("max.reps" = "reps", "exercise" = "exercise", "weight" = "weight")
      ) %>%
      distinct() %>%
      group_by(exercise, weight, max.reps) %>%
      summarise(
        count = n(),
        date = max(date)
      ) %>%
      left_join(
        session.weights(),
        # weightlifting.log,
        by = c("max.reps" = "reps", "exercise" = "exercise", "weight" = "weight", "date" = "date")
      ) %>%
      ungroup()

    temp.rep.max
  })

  weekly.tonnages <- reactive ({
    session.tonnage() %>%
    #session.tonnage %>%
      mutate(week = as.numeric(date - start.date) %/% 7) %>%
      group_by(program, week, exercise) %>%
      summarize(
        top.set = max(top.set),
        tonnage = sum(tonnage),
        weekof = min(date)
      )
  })

  exercisef <- reactive({
    temp <- factor(input$exercise, ordered = TRUE)
    temp
  })

})
