# To do:
#   Increment by body group (5% for lower, 2.5% for upper, etc.)
#   Increment by poundage instead of percentage (10 lbs for lower, 5 lbs for upper, etc.)
#   training max includes prediction for other elements (e.g., by equipment or variant)


#' Creates a programming schedule from a program template given a user's historical information.
#' @export
#'
#' @param program The name of a supported program. Supported programs can be listed with \code{\link{available_programs}}
#' @param weightlifting.log A data frame containing at least the following elements: \code{program, date, exercise, variant, reps,  weight}
#' @param smallest_plate The smallest plate available to the trainee for the duration of the program. In English units, this is usually somewhere between 1.25 and 5 pounds.
#' @param increment The percentage to increment the weights over each cycle. 2.5\% is the default, which provides a 5-lb increase on upper body lifts and a 10-lb increase on lower body lifts for a typical male lifter.
#' @param cycles The number of cycles to include in the program.
#' @param deload_every If the program supports deloads, the number of cycles between each deload.
#' @param ... Variables to be passed to the \code{\link{training_max}} function, which establishes the baseline weight for the program template
#' @return A schedule for a weightlifting program in terms of \code{cycle, day, exercise, variant, set, reps, percentage, training max, weight}

program_schedule <- function(
  program = NA,
  weightlifting.log = NA,
  smallest_plate = 2.5,
  increment = .025,
  cycles = 4,
  deload_every = 0,
  ...) {

  if (! all(! is.na(program), program %in% rweightlifting::available_programs())) {
    stop(paste0("Please provide a valid program. Choices are ", paste0(rweightlifting::available_programs(), collapse = ", "), "."))
  }

  training.max <- training_max(weightlifting.log = weightlifting.log, program = program, ...)

#  temp.program <- eval(call(program, increment_percentage = 0.05))
  temp.program <- eval(call(program))
  program.schedule <- list()

  # # First cycle
  # program.schedule <- temp.program$schedule %>%
  #   left_join(training.max, by = c("exercise", "equipment", "variant")) %>%
  #   mutate(weight = plyr::round_any(percentage * training_max, smallest_plate * 2, round)) %>%
  #   mutate(cycle = 1) %>%
  #   select(cycle, everything())

  # if (cycles > 1) {
    for (i in seq(from = 1, to = cycles)) {

      if (i > 1) { # Increment cycles 2+
        if (increment < smallest_plate * 2) { # increment is a percentage
          training.max$training_max <- round(training.max$training_max * exp(increment), 1)
        } else { # increment is added weight
          training.max$training_max <- round(training.max$training_max + increment, 1)
        }
      }

      temp.program.schedule <- temp.program$schedule %>%
        left_join(training.max, by = c("exercise", "equipment", "variant")) %>%
        mutate(weight = plyr::round_any(percentage * training_max, smallest_plate * 2, round)) %>%
        mutate(cycle = i) %>%
        select(cycle, everything())

      program.schedule <- bind_rows(program.schedule, temp.program.schedule)

      if (deload_every > 0 &
          i %% deload_every == 0 &
          ! is.null(temp.program$deload_schedule)
      ) {
        temp.program.deload <- temp.program$deload_schedule %>%
          left_join(training.max, by = c("exercise", "equipment", "variant")) %>%
          mutate(weight = plyr::round_any(percentage * training_max, smallest_plate * 2, round)) %>%
          mutate(cycle = i) %>%
          select(cycle, everything())

        program.schedule <- bind_rows(program.schedule, temp.program.deload)
      }
    }
  # }

  program.schedule
}

# deload_week <- function(
#   program = NA,
#   deload.type = NA,
#   deload.amount = 0.7
# ) {
#   # Deload week should be a function that takes a program as input
#   # Programs need a class attribute
#   if (! all(! is.na(program), program %in% rweightlifting::available_programs())) {
#     stop(paste0("Please provide a valid program. Choices are ", paste0(rweightlifting::available_programs(), collapse = ", "), "."))
#
#     # Should be able to choose more than one deload type
#     if (is.na(deload.type)) {
#       deload.type <- sample(c("cut volume", "cut intensity", "cut tonnage"), 1)
#     }
#   }
# }
