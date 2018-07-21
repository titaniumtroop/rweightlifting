# Model predicted growth curve, then plot weight plan on model

# top.set.history is a dataframe containing the top set for each date for a single exercise
predicted.vs.planned <- function(top.set.history, start.seq = 1, cycle.workouts = 24, increase.after = 2, increase.weight = 5, deload.factor = 0.85, plate = 1.25) {

  #if (nrow(top.set.history) < cycle.workouts) return()

  top.set.history <- top.set.history %>% arrange(date, exercise)

  top.set.history$workout <- 1:nrow(top.set.history)
  x = 1:(nrow(top.set.history)  + cycle.workouts)
  start.seq = nrow(top.set.history) + 1
  start.weight = max(top.set.history$top.set * deload.factor, na.rm = TRUE)

  i <- seq(from = start.seq, to = start.seq + cycle.workouts, by = 1)
  i <- as.data.frame(i)
  j <- (i - start.seq - 1) %/% increase.after * increase.weight + start.weight


  planned <- data.frame(i,j)
  names(planned) <- c("workout", "top.set")
  planned$top.set <- round_any(planned$top.set, plate * 2, floor)

  cols <- c("Guess"="red","Predicted"="green", "Observed" = "blue", "Planned" = "orange")

  observed <- ggplot(top.set.history) +
    geom_point(
      aes(workout, top.set, fill = "Observed"),
      shape = 21,
      stroke = 0,
      size = 2.5
    ) +
    xlab("workout no.") +
    ylab("weight") +
    coord_cartesian(xlim = x, ylim = 0:max(top.set.history$top.set * exp(.25))) +
    scale_color_manual(name = "Models", values = cols) +
    scale_fill_manual(name = "Values", values = cols)
  #  observed


  # predictor
  model <- lm(data = top.set.history, top.set ~ log(workout))
  #confint(model, level=0.95)
  #plot(fitted(model),residuals(model))
  y <- predict(model, newdata = list(workout = x), interval="confidence")

  observed <- observed +
    geom_line(data = data.frame(x),
              aes(x, y[,1], color = "Predicted"),
              size = 1.5,
              alpha = 0.66
    )

  observed <- observed +
    geom_point(
      data = planned,
      aes(x = workout, y = top.set, fill = "Planned"),
      shape = 21,
      stroke = 0,
      size = 2.5
    )

  observed +
    labs(
      title = unique(top.set.history$exercise)
    )
}

planned <- function(
  top.set.history,
  start.seq = 1,
  cycle.workouts = 24,
  increase.after = 2,
  increase.weight = 5,
  deload.factor = 0.85,
  sets = 5,
  reps = 5,
  ramp = 0.125,
  plate = 1.25
) {
  top.set.history <- top.set.history %>% arrange(date, exercise)

  top.set.history$workout <- 1:nrow(top.set.history)
  x = 1:(nrow(top.set.history) + cycle.workouts)
  start.seq = nrow(top.set.history) + 1
  start.weight = max(top.set.history$top.set * deload.factor, na.rm = TRUE)

  # Planned will need: workout #, set #, reps #, weight
  planned.table <- data.frame()

  for (l in 1:sets) {
    ramp.percent <- 1 - ((l - 1) * ramp)
    i <- seq(from = start.seq, to = start.seq + cycle.workouts, by = 1)
    i <- as.data.frame(i)
    j <- ((i - start.seq + 1) %/% increase.after * increase.weight + start.weight) * ramp.percent
    temp <- data.frame(i, j)
    names(temp) <- c("workout", "weight")
    temp$weight <- round_any(temp$weight, plate * 2, floor)
    temp$set <- sets - l + 1
    temp$reps <- reps
    planned.table <- rbind(planned.table, temp)
  }
  planned.table %>% arrange(workout, weight)
}

#out.table <- planned(top.set.history = session.models %>% filter(exercise == "squat"))


no.light.day.top.sets <- function(data)
{
  output <- integer(length(data))
  for(i in 1:(length(output)-1L))
  {
    output[[i]] <- (data[[i]] >= 0.85 * data[[i+1L]])
  }
  return(output)
}

