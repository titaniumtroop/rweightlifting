# Programming evaluates a program over a specific number of cycles, with a specific percentage increment after each cycle
# For each major lift type, the first cycle will use a recent projected 1-rep max (P1RM) times a deload percentage to set a training max TM for the cycle
# Subsequent cycles will use a training max that increments from the original TM
# To do:
#   Increment by body group (5% for lower, 2.5% for upper, etc.)
#   Increment by poundage instead of percentage (10 lbs for lower, 5 lbs for upper, etc.)
#   TM includes prediction for other elements (e.g., by equipment or variant)

program_schedule <- function(
  program = NA,
  weightlifting.log = NA,
  smallest_plate = 2.5,
  increment = .025,
  cycles = 4,
  ...) {

  if (! all(! is.na(program), program %in% rweightlifting::available_programs())) {
    stop(paste0("Please provide a valid program. Choices are ", paste0(rweightlifting::available_programs(), collapse = ", "), "."))
  }

  training.max <- training_max(weightlifting.log = weightlifting.log, program = program)

  temp.program <- eval(call(program))

  # First cycle
  program.schedule <- temp.program$schedule %>%
    left_join(training.max) %>%
    mutate(weight = plyr::round_any(percentage * training_max, smallest_plate * 2, round))

  for (i in seq_along(2:cycles)) {

    if (increment < smallest_plate * 2) { # increment is a percentage
      training.max$training_max <- training.max$training_max * exp(increment)
    } else { # increment is added weight
      training.max$training_max <- training.max$training_max + increment
    }

    temp.program.schedule <- temp.program$schedule %>%
      left_join(training.max) %>%
      mutate(weight = plyr::round_any(percentage * training_max, smallest_plate * 2, round))

    program.schedule <- bind_rows(program.schedule, temp.program.schedule)
  }
}
