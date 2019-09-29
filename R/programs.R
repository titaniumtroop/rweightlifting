# This file lays out common weightlifting programs
# A program contains 3 non-NULL elements
# name
# duration, in days
# a tibble showing the schedule of each set planned for a single cycle of the program
# the schedule should list the day, exercise, implement, variant, set, reps, and percentage

available_programs <- function() {
  c("madcow", "wendler_531", "wendler_531_pyramid", "base_531_bench_863", "lower_531_lsf_upper_863_pyramid")
}

lower_531_lsf_upper_863_pyramid <- function() {
  # Won't program deadload week
  lower_531_lsf_upper_863_pyramid <- list()
  lower_531_lsf_upper_863_pyramid$name <- "Lower 5-3-1 Last Set First, Upper 8-6-3 Pyramid, with Deload"
  lower_531_lsf_upper_863_pyramid$duration <- 28
  lower_531_lsf_upper_863_pyramid$RM_reps <- 1

  # 8-6-3 with Pyramid for upper body
  # Source T-Nation: https://www.t-nation.com/training/8-6-3-for-size-and-strength
  # So What Does It Look Like?
  #   Week 1	Week 2	Week 3
  # Set	Reps	Base Number	Set	Reps	Base Number	Set	Reps	Base Number
  # 1	8	65%	1	6	70%	1	8	75%
  # 2	8	75%	2	6	80%	2	6	85%
  # 3	8	80%	3	6	85%	3	3	90%

  # 5-3-1 with Last Set First
  # Source T-Nation: https://www.t-nation.com/workouts/531-how-to-build-pure-strength
  # Week 1	Week 2	Week 3	Week 4
  # Set 1	65% x 5	70% x 3	75% x 5	40% x 5
  # Set 2	75% x 5	80% x 3	85% x 3	50% x 5
  # Set 3	85% x 5+	90% x 3+	95% x 1+	60% x 5


  #base_531_bench_863$schedule <- read_csv("./data/base_531_bench_863_schedule.csv", col_types = "iccciid")
  lower_531_lsf_upper_863_pyramid$schedule <- tibble(
    day = c(
      rep(1L, 5),
      rep(2L, 4),
      rep(4L, 5),
      rep(6L, 4),
      rep(8L, 5),
      rep(9L, 4),
      rep(11L, 5),
      rep(13L, 4),
      rep(15L, 5),
      rep(16L, 4),
      rep(18L, 5),
      rep(20L, 4),
      rep(22L, 3),
      rep(23L, 3),
      rep(25L, 3),
      rep(27L, 3)
    ),
    exercise = c(
      rep(c(
        rep("bench", 5),
        rep("squat", 4),
        rep("press", 5),
        rep("deadlift", 4)
      ), 3),
      rep("bench", 3),
      rep("squat", 3),
      rep("press", 3),
      rep("deadlift", 3)
    ),
    equipment = rep("barbell", 66),
    variant = c(
      rep(c(
        rep("flat", 5),
        rep("low bar", 4),
        rep("overhead", 5),
        rep("conventional", 4)
      ), 3),
      rep("flat", 3),
      rep("low bar", 3),
      rep("overhead", 3),
      rep("conventional", 3)
    ),
    set = c(
      rep(
        c(
        1:5,
        1:4,
        1:5,
        1:4
      ), 3),
      rep(1:3, 4)
    ),
    reps = c(
      rep(8L, 5),
      rep(5L, 4),
      rep(8L, 5),
      rep(5L, 4),
      rep(6L, 5),
      rep(3L, 4),
      rep(6L, 5),
      rep(3L, 4),
      rep(3L, 5),
      rep(1L, 4),
      rep(3L, 5),
      rep(1L, 4),
      rep(5L, 3 * 4)
    ),
    percentage = c(
      0.65, 0.75, 0.8, 0.75, 0.65,
      0.65, 0.75, 0.85, 0.65,
      0.65, 0.75, 0.8, 0.75, 0.65,
      0.65, 0.75, 0.85, 0.65,
      0.7, 0.8, 0.85, 0.8, 0.7,
      0.7, 0.8, 0.9, 0.7,
      0.7, 0.8, 0.85, 0.8, 0.7,
      0.7, 0.8, 0.9, 0.7,
      0.75, 0.85, 0.9, 0.85, 0.75,
      0.75, 0.85, 0.95, 0.75,
      0.75, 0.85, 0.9, 0.85, 0.75,
      0.75, 0.85, 0.95, 0.75,
      0.45, 0.55, 0.65,
      0.45, 0.55, 0.65,
      0.45, 0.55, 0.65,
      0.45, 0.55, 0.65
    )
)

  lower_531_lsf_upper_863_pyramid

}

base_531_bench_863 <- function() {
  # Won't program deadload week
  base_531_bench_863 <- list()
  base_531_bench_863$name <- "Base 5-3-1, Bench 8-6-3"
  base_531_bench_863$duration <- 21
  base_531_bench_863$RM_reps <- 1

  # 8-6-3 with Pyramid for upper body
  # Source T-Nation: https://www.t-nation.com/training/8-6-3-for-size-and-strength
  # So What Does It Look Like?
  #   Week 1	Week 2	Week 3
  # Set	Reps	Base Number	Set	Reps	Base Number	Set	Reps	Base Number
  # 1	8	65%	1	6	70%	1	8	75%
  # 2	8	75%	2	6	80%	2	6	85%
  # 3	8	80%	3	6	85%	3	3	90%

  # 5-3-1 with Last Set First
  # Source T-Nation: https://www.t-nation.com/workouts/531-how-to-build-pure-strength
  # Week 1	Week 2	Week 3	Week 4
  # Set 1	65% x 5	70% x 3	75% x 5	40% x 5
  # Set 2	75% x 5	80% x 3	85% x 3	50% x 5
  # Set 3	85% x 5+	90% x 3+	95% x 1+	60% x 5


  #base_531_bench_863$schedule <- read_csv("./data/base_531_bench_863_schedule.csv", col_types = "iccciid")
  base_531_bench_863$schedule <- structure(list(
    day = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L, 6L, 6L, 6L, 6L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 11L, 11L, 11L, 11L, 11L, 13L, 13L, 13L, 13L, 15L, 15L, 15L, 15L, 15L, 16L, 16L, 16L, 16L, 18L, 18L, 18L, 18L, 18L, 20L, 20L, 20L, 20L),
    exercise = c("bench", "bench", "bench", "bench", "bench", "squat", "squat", "squat", "squat", "press", "press", "press", "press", "press", "deadlift", "deadlift", "deadlift", "deadlift", "bench", "bench", "bench", "bench", "bench", "squat", "squat", "squat", "squat", "press", "press", "press", "press", "press", "deadlift", "deadlift", "deadlift", "deadlift", "bench", "bench", "bench", "bench", "bench", "squat", "squat", "squat", "squat", "press", "press", "press", "press", "press", "deadlift", "deadlift", "deadlift", "deadlift"),
    equipment = c("barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell", "barbell"),
    variant = c("flat", "flat", "flat", "flat", "flat", "low bar", "low bar", "low bar", "low bar", "overhead", "overhead", "overhead", "overhead", "overhead", "conventional", "conventional", "conventional", "conventional", "flat", "flat", "flat", "flat", "flat", "low bar", "low bar", "low bar", "low bar", "overhead", "overhead", "overhead", "overhead", "overhead", "conventional", "conventional", "conventional", "conventional", "flat", "flat", "flat", "flat", "flat", "low bar", "low bar", "low bar", "low bar", "overhead", "overhead", "overhead", "overhead", "overhead", "conventional", "conventional", "conventional", "conventional"),
    set = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L),  reps = c(8L, 8L, 8L, 8L, 8L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 5L, 3L, 1L, 5L, 5L, 3L, 1L, 3L, 5L, 5L, 3L, 1L, 5L),
    percentage = c(0.65, 0.75, 0.8, 0.75, 0.65, 0.65, 0.75, 0.85, 0.65, 0.65, 0.75, 0.85, 0.75, 0.65, 0.65, 0.75, 0.85, 0.65, 0.7, 0.8, 0.85, 0.8, 0.7, 0.7, 0.8, 0.9, 0.7, 0.7, 0.8, 0.9, 0.8, 0.7, 0.7, 0.8, 0.9, 0.7, 0.75, 0.85, 0.9, 0.85, 0.75, 0.75, 0.85, 0.95, 0.75, 0.75, 0.85, 0.95, 0.85, 0.75, 0.75, 0.85, 0.95, 0.75)),
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"
    ),
    row.names = c(NA, -54L),
    spec = structure(list(cols = list(
      day = structure(list(), class = c("collector_integer", "collector" )),
      exercise = structure(list(), class = c("collector_character", "collector")),
      equipment = structure(list(), class = c("collector_character", "collector")),
      variant = structure(list(), class = c("collector_character", "collector")),
      set = structure(list(), class = c("collector_integer", "collector")),
      reps = structure(list(), class = c("collector_integer", "collector")),
      percentage = structure(list(), class = c("collector_double", "collector"))),
      default = structure(list(), class = c("collector_guess", "collector")), skip = 1), class = "col_spec"))

  base_531_bench_863

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

