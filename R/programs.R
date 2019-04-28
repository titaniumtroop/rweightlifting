# This file lays out common weightlifting programs
# A program contains 3 non-NULL elements
# name
# duration, in days
# a tibble showing the schedule of each set planned for a single cycle of the program
# the schedule should list the day, exercise, implement, variant, set, reps, and percentage

available_programs <- function() {
  c("madcow", "wendler_531", "wendler_531_pyramid")
}

madcow <- function(set_interval = .125, increment_percentage = 0.025) {
  # Sets out a single week's cycle for the Madcow 5x5 program
  madcow <- list()
  madcow$name <- "Madcow 5x5"
  madcow$duration <- 7
  madcow$RM_reps <- 5

  day <- c(
    rep(1, 15),
    rep(3, 12),
    rep(5, 18)
  )
  exercise <- c(
    rep("squat", 5),
    rep("bench", 5),
    rep("row", 5),
    rep("squat", 4),
    rep("press", 4),
    rep("deadlift", 4),
    rep("squat", 6),
    rep("bench", 6),
    rep("row", 6)
  )

  equipment <- c(
    rep("barbell", 45)
  )

  variant <- c(
    rep("low bar", 5),
    rep("flat", 5),
    rep("pendlay", 5),
    rep("low bar", 4),
    rep("overhead", 4),
    rep("conventional", 4),
    rep("low bar", 6),
    rep("flat", 6),
    rep("pendlay", 6)
  )

  set <- c(
    1:5, 1:5, 1:5, 1:4, 1:4, 1:4, 1:6, 1:6, 1:6
  )

  reps <- c(
    rep(5, 15),
    rep(5, 12),
    rep(c(rep(5, 4), 3, 8), 3)
  )

  percentage <- c(
    rep(c(
      1 - set_interval * 4,
      1 - set_interval * 3,
      1 - set_interval * 2,
      1 - set_interval * 1,
      1 - set_interval * 0
    ), 3),
    rep(c(
      1 - set_interval * 4,
      1 - set_interval * 3,
      1 - set_interval * 2,
      1 - set_interval * 2
    ), 3),
    rep(c(
      1 - set_interval * 4,
      1 - set_interval * 3,
      1 - set_interval * 2,
      1 - set_interval * 1,
      1 + increment_percentage, # Will raise this to the cycle power (^ cycle)
      1 - set_interval * 2
    ), 3)
  )

  madcow$schedule <- tibble(day, exercise, equipment, variant, set, reps, percentage)
  madcow
}

wendler_531 <- function() {
  # Sets out a four-week cycle for the Wendler 5-3-1 program
  wendler_531 <- list()
  wendler_531$name <- "Wendler 5-3-1"
  wendler_531$duration <- 28
  wendler_531$RM_reps <- 1

  day <- c(
    rep(1, 3),
    rep(2, 3),
    rep(4, 3),
    rep(5, 3),
    rep(8, 3),
    rep(9, 3),
    rep(11, 3),
    rep(12, 3),
    rep(15, 3),
    rep(16, 3),
    rep(18, 3),
    rep(19, 3),
    rep(22, 3),
    rep(23, 3),
    rep(25, 3),
    rep(26, 3)
  )

  exercise <- c(
    rep("bench", 3),
    rep("squat", 3),
    rep("press", 3),
    rep("deadlift", 3),
    rep("bench", 3),
    rep("squat", 3),
    rep("press", 3),
    rep("deadlift", 3),
    rep("bench", 3),
    rep("squat", 3),
    rep("press", 3),
    rep("deadlift", 3),
    rep("bench", 3),
    rep("squat", 3),
    rep("press", 3),
    rep("deadlift", 3)
  )

  equipment <- c(
    rep("barbell", 48)
  )

  variant <- c(
    rep("flat", 3),
    rep("low bar", 3),
    rep("overhead", 3),
    rep("conventional", 3),
    rep("flat", 3),
    rep("low bar", 3),
    rep("overhead", 3),
    rep("conventional", 3),
    rep("flat", 3),
    rep("low bar", 3),
    rep("overhead", 3),
    rep("conventional", 3),
    rep("flat", 3),
    rep("low bar", 3),
    rep("overhead", 3),
    rep("conventional", 3)
  )

  set <- c(
    rep(1:3, 16)
  )

  reps <- c(
    rep(5, 12),
    rep(3, 12),
    rep(c(5,3,1), 4),
    rep(5, 12)
  )

  percentage <- c(
    rep(c(.65, .75, .85), 4),
    rep(c(.70, .80, .90), 4),
    rep(c(.75, .85, .95), 4),
    rep(c(.40, .50, .60), 4)
  )

  wendler_531$schedule <- tibble(day, exercise, equipment, variant, set, reps, percentage)
  wendler_531
}

wendler_531_pyramid <- function() {
  # Sets out a four-week cycle for the Wendler 5-3-1 program
  wendler_531_pyramid <- list()
  wendler_531_pyramid$name <- "Wendler 5-3-1 Pyramid"
  wendler_531_pyramid$duration <- 28
  wendler_531_pyramid$RM_reps <- 1

  day <- c(
    rep(1, 5),
    rep(2, 5),
    rep(4, 5),
    rep(5, 5),
    rep(8, 5),
    rep(9, 5),
    rep(11, 5),
    rep(12, 5),
    rep(15, 5),
    rep(16, 5),
    rep(18, 5),
    rep(19, 5),
    rep(22, 5),
    rep(23, 5),
    rep(25, 5),
    rep(26, 5)
  )

  exercise <- c(
    rep("bench", 5),
    rep("squat", 5),
    rep("press", 5),
    rep("deadlift", 5),
    rep("bench", 5),
    rep("squat", 5),
    rep("press", 5),
    rep("deadlift", 5),
    rep("bench", 5),
    rep("squat", 5),
    rep("press", 5),
    rep("deadlift", 5),
    rep("bench", 5),
    rep("squat", 5),
    rep("press", 5),
    rep("deadlift", 5)
  )

  equipment <- c(
    rep("barbell", 80)
  )

  variant <- c(
    rep("flat", 5),
    rep("low bar", 5),
    rep("overhead", 5),
    rep("conventional", 5),
    rep("flat", 5),
    rep("low bar", 5),
    rep("overhead", 5),
    rep("conventional", 5),
    rep("flat", 5),
    rep("low bar", 5),
    rep("overhead", 5),
    rep("conventional", 5),
    rep("flat", 5),
    rep("low bar", 5),
    rep("overhead", 5),
    rep("conventional", 5)
  )

  set <- c(
    rep(1:5, 16)
  )

  reps <- c(
    rep(5, 20),
    rep(3, 20),
    rep(c(5,3,1,3,5), 4),
    rep(5, 20)
  )

  percentage <- c(
    rep(c(.65, .75, .85, .75, .65), 4),
    rep(c(.70, .80, .90, .80, .70), 4),
    rep(c(.75, .85, .95, .85, .75), 4),
    rep(c(.40, .50, .60, .50, .40), 4)
  )

  wendler_531_pyramid$schedule <- tibble(day, exercise, equipment, variant, set, reps, percentage)

  wendler_531_pyramid
}

# Extracts the programming elements of a program list and returns them in a dataframe.
# Only necessary if programming schedule is in list format, rather than data frame
program_df <- function(program = NULL) {

  if (is.null(program)) {
    stop("Please supply a valid program.")
  }

  if (! all(c("day", "exercise", "set", "reps", "percentage") %in% names(program))) {
    stop("Please supply a valid program having at least day, exercise, set, reps, and percentage specified.")
  }

  programming.columns <- lengths(program) > 1 # list elements containing a 1-element named vector are program discriptors
  programming <- program[programming.columns] # So we're going to exlcude them from the vectors describing a program

  # Check to make sure all column lengths are the same; if not, return error
  if (all(diff(lengths(programming))) == 0) {
    return(data.frame(programming))
  } else {
    stop("All programming columns must be the same length.")
  }
}

