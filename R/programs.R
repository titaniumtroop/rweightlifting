# This file lays out common weightlifting programs
# A program contains 3 non-NULL elements
# name
# duration, in days
# a tibble showing the schedule of each set planned for a single cycle of the program
# the schedule should list the day, exercise, implement, variant, set, reps, and percentage
# an optional deload duration and deload schedule can be used to program deloads every few cycles

#' @importFrom magrittr %>%
magrittr::`%>%`

#' List of available weightlifting programming templates
#' @export
#'
#' @return A character vector of available programs
#' @examples
#' "novice_linear_progression" %in% available_programs()

available_programs <- function() {
  c("novice_linear_progression",
    "four_day_LP",
    "madcow",
    "wendler_531",
    "wendler_531_pyramid",
    "base_531_bench_863",
    "lower_531_lsf_upper_863_pyramid"
  )
}


#' Programming template for Lower 5-3-1 Last Set First, Upper 8-6-3 Pyramid
#' @export
#'
#' @param ... Some program functions take arguments for percentage schemes; the ellipsis lets other programming functions accept those arguments without tripping error checks.
#' @return a list of template elements, including \code{name, duration, rep-max} on which percentages are based, \code{schedule}. Also has a \code{deload_duration, deload_schedule}, if desired.

lower_531_lsf_upper_863_pyramid <- function(...) {
  program <- list()
  program$name <- "Lower 5-3-1 Last Set First, Upper 8-6-3 Pyramid"
  program$duration <- 21
  program$sessions_per_week <- 4
  program$deload_duration <- 7
  program$RM_reps <- 1

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
  program$schedule <- tibble::tibble(
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
      rep(20L, 4)
    ),
    exercise = c(
      rep(c(
        rep("bench", 5),
        rep("squat", 4),
        rep("press", 5),
        rep("deadlift", 4)
      ), 3)
    ),
    equipment = rep("barbell", 54),
    variant = c(
      rep(c(
        rep("flat", 5),
        rep("low bar", 4),
        rep("overhead", 5),
        rep("conventional", 4)
      ), 3)
    ),
    set = c(
      rep(
        c(
        1:5,
        1:4,
        1:5,
        1:4
      ), 3)
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
      rep(1L, 4)
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
      0.75, 0.85, 0.95, 0.75
    )
)

  program$deload_schedule <- tibble::tibble(
    day = c(
      rep(22, 5),
      rep(23, 4),
      rep(25, 5),
      rep(26, 4)
    ),
    exercise = c(
      rep("bench", 5),
      rep("squat", 4),
      rep("press", 5),
      rep("deadlift", 4)
    ),
    equipment = c(
      rep("barbell", 18)
    ),
    variant = c(
      rep("flat", 5),
      rep("low bar", 4),
      rep("overhead", 5),
      rep("conventional", 4)
    ),
    set = c(1:5, 1:4, 1:5, 1:4),
    reps = c(
      rep(5, 18)
    ),
    percentage = c(
      c(.45, .55, .65, .65, .65),
      c(.45, .55, .65, .65),
      c(.45, .55, .65, .65, .65),
      c(.45, .55, .65, .65)
    )
  )

  program
}


#' Programming template for Base 5-3-1, Bench 8-6-3
#' @export
#'
#' @param ... Some program functions take arguments for percentage schemes; the ellipsis lets other programming functions accept those arguments without tripping error checks.
#' @return a list of template elements, including \code{name, duration, rep-max} on which percentages are based, \code{schedule}. Also has a \code{deload_duration, deload_schedule}, if desired.

base_531_bench_863 <- function(...) {
  program <- list()
  program$name <- "Base 5-3-1, Bench 8-6-3"
  program$duration <- 21
  program$sessions_per_week <- 4
  program$deload_duration <- 7
  program$RM_reps <- 1

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


  #program$schedule <- read_csv("./data/base_531_bench_863_schedule.csv", col_types = "iccciid")
  program$schedule <- structure(list(
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

  program$deload_schedule <- tibble::tibble(
    day = c(
      rep(22, 3),
      rep(23, 3),
      rep(25, 3),
      rep(26, 3)
    ),
    exercise = c(
      rep("bench", 3),
      rep("squat", 3),
      rep("press", 3),
      rep("deadlift", 3)
    ),
    equipment = c(
      rep("barbell", 12)
    ),
    variant = c(
      rep("flat", 3),
      rep("low bar", 3),
      rep("overhead", 3),
      rep("conventional", 3)
    ),
    set = c(
      rep(1:3, 4)
    ),
    reps = c(
      rep(5, 12)
    ),
    percentage = c(
      rep(c(.40, .50, .60), 4)
    )
  )


  program

}


#' Programming template for Madcow 5x5
#' @export
#'
#' @param set_interval the percentage to ramp between sets; lower values are harder
#' @param increment_percentage the increase in weight percentage on the heavy day of the cycle
#' @param ... Some program functions take arguments for percentage schemes; the ellipsis lets other programming functions accept those arguments without tripping error checks.
#' @return a list of template elements, including \code{name, duration, rep-max} on which percentages are based, \code{schedule}.

madcow <- function(set_interval = .125, increment_percentage = 0.025, ...) {
  # Sets out a single week's cycle for the Madcow 5x5 program
  program <- list()
  program$name <- "Madcow 5x5"
  program$sessions_per_week <- 3
  program$duration <- 7
  program$RM_reps <- 5

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

  program$schedule <- tibble::tibble(day, exercise, equipment, variant, set, reps, percentage)
  program
}


#' Programming template for Novice Linear Progression
#' @export
#'
#' @param increment_percentage the increase in weight percentage on subsequent days in the cycle
#' @param ... Some program functions take arguments for percentage schemes; the ellipsis lets other programming functions accept those arguments without tripping error checks.
#' @return a list of template elements, including \code{name, duration, rep-max} on which percentages are based, \code{schedule}. Note that this NLP is based on a a single 4-day cycle; most NLPs consist of repeating an A/B exercise three times per week, or 3 cycles over a two-week period. This cycle can be converted to a MWF format using the \code{adjust_schedule} function.

novice_linear_progression <- function(increment_percentage = 0.025, ...) {
  # Sets out a single A/B cycle for NLP
  # Runs on a 12-day period, not 14 (i.e., every other day)
  program <- list()
  program$name <- "Novice Linear Progression"
  program$sessions_per_week <- 3
  program$duration <- 4
  program$RM_reps <- 5

  program$schedule <- tibble::tibble(
    day = c(
      rep(1, 7),
      rep(3, 7)
    ),
    exercise = rep(c(
        rep("squat", 3),
        rep("bench", 3),
        rep("deadlift", 1),
        rep("squat", 3),
        rep("press", 3),
        rep("deadlift", 1)
      ), 1),
    equipment = rep("barbell", 14),
    variant = rep(c(
        rep("low bar", 3),
        rep("flat", 3),
        rep("conventional", 1),
        rep("low bar", 3),
        rep("overhead", 3),
        rep("conventional", 1)
      ), 1),
    set = rep(c(
      1:3, 1:3, 1
    ), 2),
    reps = rep(5, 14),
    percentage = c(
      rep(1, 7),
      round(rep(1 * exp(increment_percentage / 2), 3), 4),
      rep(1, 3),
      round(1 * exp(increment_percentage / 2), 4)
    )
  )

  program
}


#' Programming template for 4-Day Linear Progression
#' @export
#'
#' @param ... Some program functions take arguments for percentage schemes; the ellipsis lets other programming functions accept those arguments without tripping error checks.
#' @return a list of template elements, including \code{name, duration, rep-max} on which percentages are based, \code{schedule}.

four_day_LP <- function(...) {
  # Sets out a single week's cycle for a 4-day LP split
  # One day a week will be a 3x5 intensity day for each exercise (1x5 for deadlift)
  # One day a week will be a 5x5 volume day for each exercise
  program <- list()
  program$name <- "4-Day Linear Progression"
  program$sessions_per_week <- 4
  program$duration <- 7
  program$RM_reps <- 5

  program$schedule <- tibble::tibble(
    day = c(
      rep(1, 8),
      rep(2, 8),
      rep(2, 8),
      rep(2, 6)
    ),
    exercise = c(
      rep("bench", 3),
      rep("press", 5),
      rep("squat", 3),
      rep("deadlift", 5),
      rep("press", 3),
      rep("bench", 5),
      rep("deadlift", 1),
      rep("squat", 5)
    ),
    equipment = rep("barbell", 30),
    variant = c(
      rep("flat", 3),
      rep("overhead", 5),
      rep("low bar", 3),
      rep("conventional", 5),
      rep("overhead", 3),
      rep("flat", 5),
      rep("conventional", 1),
      rep("low bar", 5)
    ),
    set = c(
      1:3, 1:5, 1:3, 1:5, 1:3, 1:5, 1, 1:5
    ),
    reps = rep(5, 30),
    percentage = c(
      rep(1, 3),
      rep(0.8, 5),
      rep(1, 3),
      rep(0.8, 5),
      rep(1, 3),
      rep(0.8, 5),
      1,
      rep(0.8, 5)
    )
  )

  program
}


#' Programming template for Wendler 5-3-1
#' @export
#'
#' @param ... Some program functions take arguments for percentage schemes; the ellipsis lets other programming functions accept those arguments without tripping error checks.
#' @return a list of template elements, including \code{name, duration, rep-max} on which percentages are based, \code{schedule}. Also has a \code{deload_duration, deload_schedule}, if desired.

wendler_531 <- function(...) {
  # Sets out a 3-week cycle for the Wendler 5-3-1 program
  # Deloads added separately
  program <- list()
  program$name <- "Wendler 5-3-1"
  program$duration <- 21
  program$sessions_per_week <- 4
  program$deload_duration <- 7
  program$RM_reps <- 1

  program$schedule <- tibble::tibble(
    day = c(
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
      rep(19, 3)
    ),
    exercise = c(
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
    ),
    equipment = c(
      rep("barbell", 36)
    ),
    variant = c(
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
    ),
    set = c(
      rep(1:3, 12)
    ),
    reps = c(
      rep(5, 12),
      rep(3, 12),
      rep(c(5,3,1), 4)
    ),
    percentage = c(
      rep(c(.65, .75, .85), 4),
      rep(c(.70, .80, .90), 4),
      rep(c(.75, .85, .95), 4)
    )
  )

  program$deload_schedule <- tibble::tibble(
    day = c(
      rep(22, 3),
      rep(23, 3),
      rep(25, 3),
      rep(26, 3)
    ),
    exercise = c(
      rep("bench", 3),
      rep("squat", 3),
      rep("press", 3),
      rep("deadlift", 3)
    ),
    equipment = c(
      rep("barbell", 12)
    ),
    variant = c(
      rep("flat", 3),
      rep("low bar", 3),
      rep("overhead", 3),
      rep("conventional", 3)
    ),
    set = c(
      rep(1:3, 4)
    ),
    reps = c(
      rep(5, 12)
    ),
    percentage = c(
      rep(c(.40, .50, .60), 4)
    )
  )

  program
}


#' Programming template for Wendler 5-3-1 Pyramid
#' @export
#'
#' @param ... Some program functions take arguments for percentage schemes; the ellipsis lets other programming functions accept those arguments without tripping error checks.
#' @return a list of template elements, including \code{name, duration, rep-max} on which percentages are based, \code{schedule}. Also has a \code{deload_duration, deload_schedule}, if desired.

wendler_531_pyramid <- function(...) {
  # Sets out a 3-week cycle for the Wendler 5-3-1 program
  program <- list()
  program$name <- "Wendler 5-3-1 Pyramid"
  program$duration <- 21
  program$sessions_per_week <- 4
  program$deload_duration <- 7
  program$RM_reps <- 1

  program$schedule <- tibble::tibble(
    day = c(
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
      rep(19, 5)
    ),
    exercise = c(
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
    ),
    equipment = c(
      rep("barbell", 60)
    ),
    variant = c(
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
    ),
    set = c(
      rep(1:5, 12)
    ),
    reps = c(
      rep(5, 20),
      rep(3, 20),
      rep(c(5,3,1,3,5), 4)
    ),
    percentage = c(
      rep(c(.65, .75, .85, .75, .65), 4),
      rep(c(.70, .80, .90, .80, .70), 4),
      rep(c(.75, .85, .95, .85, .75), 4)
    )
  )

  program$deload_schedule <- tibble::tibble(
    day = c(
      rep(22, 3),
      rep(23, 3),
      rep(25, 3),
      rep(26, 3)
    ),
    exercise = c(
      rep("bench", 3),
      rep("squat", 3),
      rep("press", 3),
      rep("deadlift", 3)
    ),
    equipment = c(
      rep("barbell", 12)
    ),
    variant = c(
      rep("flat", 3),
      rep("low bar", 3),
      rep("overhead", 3),
      rep("conventional", 3)
    ),
    set = c(
      rep(1:3, 4)
    ),
    reps = c(
      rep(5, 12)
    ),
    percentage = c(
      rep(c(.40, .50, .60), 4)
    )
  )

  program
}

# Extracts the programming elements of a program list and returns them in a dataframe.
# Only necessary if programming schedule is in list format, rather than data frame
# program_df <- function(program = NULL) {
#
#   if (is.null(program)) {
#     stop("Please supply a valid program.")
#   }
#
#   if (! all(c("day", "exercise", "set", "reps", "percentage") %in% names(program))) {
#     stop("Please supply a valid program having at least day, exercise, set, reps, and percentage specified.")
#   }
#
#   programming.columns <- lengths(program) > 1 # list elements containing a 1-element named vector are program discriptors
#   programming <- program[programming.columns] # So we're going to exlcude them from the vectors describing a program
#
#   # Check to make sure all column lengths are the same; if not, return error
#   if (all(diff(lengths(programming))) == 0) {
#     return(data.frame(programming))
#   } else {
#     stop("All programming columns must be the same length.")
#   }
# }

