#' @importFrom rlang .data

# To do:
#   Increment by body group (5% for lower, 2.5% for upper, etc.)
#   Increment by poundage instead of percentage (10 lbs for lower, 5 lbs for upper, etc.)
#   training max includes prediction for other elements (e.g., by equipment or variant)


#' @title Program scheduling
#' @description Creates a programming schedule from a program template given a user's historical information.
#'
#' @param program The name of a supported program. Supported programs can be listed with \code{\link{available_programs}}
#' @param weightlifting.log A data frame containing at least the following elements: \code{program, date, exercise, variant, reps,  weight}
#' @param smallest_plate The smallest plate available to the trainee for the duration of the program. In English units, this is usually somewhere between 1.25 and 5 pounds.
#' @param increment The percentage to increment the weights over each cycle. 2.5\% is the default, which provides a 5-lb increase on upper body lifts and a 10-lb increase on lower body lifts for a typical male lifter.
#' @param cycles The number of cycles to include in the program.
#' @param deload_every If the program supports deloads, the number of cycles between each deload.
#' @param percentage Training max percentage. Defaults to 0.90.
#' @param prgm_start_date Date to start the program. Defaults to tomorrow.
#' @param ... Variables to be passed to the \code{\link{training_max}} function, which establishes the baseline weight for the program template
#' @return A schedule for a weightlifting program in terms of \code{cycle, day, exercise, variant, set, reps, percentage, training max, weight}
#'
#' @export

program_schedule <- function(
  program = NA,
  weightlifting.log = NA,
  smallest_plate = 2.5,
  increment = .025,
  cycles = 4,
  deload_every = 0,
  percentage = 0.90,
  prgm_start_date = Sys.Date() + 1,
  ...) {

  if (! all(! is.na(program), program %in% rweightlifting::available_programs())) {
    stop(paste0("Please provide a valid program. Choices are ", paste0(rweightlifting::available_programs(), collapse = ", "), "."))
  }

  training.max <- training_max(weightlifting.log = weightlifting.log, program = program, percentage = percentage, ...)

#  temp.program <- eval(call(program, increment_percentage = 0.05))
  temp.program <- eval(call(program, increment_percentage = increment))
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
        mutate_if(is.factor, as.character) %>%
        left_join(training.max, by = c("exercise", "equipment", "variant")) %>%
        mutate(weight = plyr::round_any(percentage * .data$training_max, smallest_plate * 2, round)) %>%
        mutate(cycle = i) %>%
        select(.data$cycle, everything())

      program.schedule <- bind_rows(program.schedule, temp.program.schedule)

      if (deload_every > 0 &
          i %% deload_every == 0 &
          ! is.null(temp.program$deload_schedule)
      ) {
        temp.program.deload <- temp.program$deload_schedule %>%
          mutate_if(is.factor, as.character) %>%
          left_join(training.max, by = c("exercise", "equipment", "variant")) %>%
          mutate(weight = plyr::round_any(percentage * .data$training_max, smallest_plate * 2, round)) %>%
          mutate(cycle = i) %>%
          select(.data$cycle, everything())

        program.schedule <- bind_rows(program.schedule, temp.program.deload)
      }
    }
  # }

  attr(program.schedule, "deload_every") <- deload_every
  attr(program.schedule, "program") <- temp.program
  attr(program.schedule, "increment") <- increment
  attr(program.schedule, "cycles") <- cycles
  attr(program.schedule, "percentage") <- percentage
  # program.schedule

  duration <- temp.program$duration[1]
  deload_duration <- temp.program$deload_duration[1]

  if (deload_every > 0 &
      ! is.null(temp.program$deload_schedule)
  ) {
    program.schedule <- program.schedule %>%
      mutate(
        date = as.Date(prgm_start_date) + (.data$cycle - 1) * duration + (deload_duration * ((.data$cycle - 1) %/% as.numeric(deload_every))) + (.data$day - 1)
      )
  } else {
    program.schedule <- program.schedule %>%
      mutate(
        date = as.Date(prgm_start_date) + (.data$cycle - 1) * duration + (.data$day - 1)
      )
  }

  program.schedule %>%
    mutate(
      cycle = as.integer(.data$cycle),
      day = as.integer(.data$day),
      reps = as.integer(.data$reps)
    ) %>%
    select(.data$cycle, .data$day, .data$date, everything())

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

#' @title Adjust program schedule dates
#' @description Modifies a program schedule to only include days specified by user
#'
#' @param program_schedule A program schedule created with the program_schedule function
#' @param allowed_days An integer vector specifying which days of the week are allowable.
#' @return A schedule for a weightlifting program in terms of \code{cycle, day, date, exercise, variant, set, reps, percentage, training max, weight}
#' @export
#'
adjust_schedule <- function(
  program_schedule = NA,
  allowed_days = c(1, 3, 5)
) {
  if (! all(! is.na(program_schedule))) {
    stop(paste0("Please provide a valid program schedule."))
  }
  if (! is.numeric(allowed_days)) {
    stop(paste0("Please provide a vector of days on which you would like to lift -- Monday = 1, Sunday = 7 -- so MWF would be c(1, 3, 5)."))
  }

  # cldr_schedule <- calendar_schedule(prgm_schedule, prgm_start_date = as.Date("2020-05-04"))
  program_schedule$new_date <- program_schedule$date

  for (i in 1:NROW(program_schedule)) {
    if (i > 1) {
      if (program_schedule$date[i] == program_schedule$date[i - 1]) { # If date has already been adjusted ahead
        program_schedule$new_date[i] <- program_schedule$new_date[i - 1]
      } else {
        program_schedule$new_date[i] <- program_schedule$new_date[i - 1] + 1
      }
    }
    j <- as.numeric(strftime(program_schedule$new_date[i],'%u'))
        # Code to find next day %in% allowed_days here
        # Increment day of week until you find one that's in allowed_days
        # If at 7, go back to 0
        # Add the number of increments to cldr_schedule$day[i]
        # Set last_day to cldr_schedule$day[i]
    duration <- 0
    while(! j %in% allowed_days) {
      if (j == 7) { j = 0 }
      j = j + 1
      duration <- duration + 1
    }
    program_schedule$new_date[i] <- program_schedule$new_date[i] + duration
  }

  program_schedule$date <- program_schedule$new_date
  program_schedule$new_date <- NULL

  program_schedule
}
