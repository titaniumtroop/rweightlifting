# These functions allow estimates of one-rep maxes using various formulas

# Formulas that have been hardcoded below
rep_max_formulas <- c("epley", "brzycki", "mcglothin", "lombardi", "mayhew", "oconner", "wathan")

# Provides one-rep max given a single weight and rep
# Vectorized
one_rep_max <- function(weight, reps, method = "epley") {

  method <- as.character(method)
  my_data <- data.frame(weight = as.numeric(weight), reps = as.numeric(reps), temp_maxes = 0)

  message(paste0("Using method '", method, "' to calculate 1-rep max."))

  are.reps.negative <- my_data$reps < 0

  if (any(are.reps.negative) == TRUE) {
    stop("Reps must be greater than or equal to 0.")
  }

  if (method == "epley") my_data$temp_maxes <- round(digits = 1,
                                      x = weight * (1 + (reps / 30))
  ) else if (method == "brzycki") my_data$temp_maxes <- round(digits = 1,
                                        x = weight * (36 / (37 - reps))
  ) else if (method == "mcglothin") my_data$temp_maxes <- round(digits = 1,
                                          x = 100 * weight / (101.3 - 2.67123 * reps)
  ) else if (method == "lombardi") my_data$temp_maxes <- round(digits = 1,
                                         x = weight * reps ^ 0.10
  ) else if (method == "mayhew") my_data$temp_maxes <- round(digits = 1,
                                       x = 100 * weight / (52.2 + 41.9 * exp(-0.055 * reps))
  ) else if (method == "oconner") my_data$temp_maxes <- round(digits = 1,
                                        x = weight * (1 + reps / 40)
  ) else if (method == "wathan") my_data$temp_maxes <- round(digits = 1,
                                       x = 100 * weight / (48.8 + 53.8 * exp(-0.075 * reps))
  ) else stop("Unknown method specified.")

  # Now we have a vector calculated for all reps >= 0. We need to adjust the calculation for reps == [0 1]
  my_data$temp_maxes <- ifelse(my_data$reps == 1,
                               my_data$weight,
                               ifelse(my_data$reps == 0, 0, my_data$temp_maxes)
  )

  return(my_data$temp_maxes)
}

# Provides n-rep max given a one-rep max and the desired rep max
# Vectorized for use in tidyverse pipelines
n_rep_max <- function(one_RM, reps, method = "epley") {

  method <- as.character(method)
  my_data <- data.frame(one_RM = as.numeric(one_RM), reps = as.numeric(reps), temp_maxes = 0)

  message(paste0("Using method '", method, "' to calculate n-rep max."))

  are.reps.positive <- my_data$reps > 0

  if (all(are.reps.positive) != TRUE) {
    stop("Reps must be greater than 0.")
  }

  if (method == "epley") my_data$temp_maxes <- round(digits = 1,
                                      x = one_RM / (1 + (reps / 30))
  ) else if (method == "brzycki") my_data$temp_maxes <- round(digits = 1,
                                                x = one_RM / (36 / (37 - reps))
  ) else if (method == "mcglothin") my_data$temp_maxes <- round(digits = 1,
                                                  x = one_RM * (101.3 - 2.67123 * reps) / 100
  ) else if (method == "lombardi") my_data$temp_maxes <- round(digits = 1,
                                                 x = one_RM / reps ^ 0.10
  ) else if (method == "mayhew") my_data$temp_maxes <- round(digits = 1,
                                               x = one_RM * (52.2 + 41.9 * exp(-0.055 * reps)) / 100
  ) else if (method == "oconner") my_data$temp_maxes <- round(digits = 1,
                                                x = one_RM / (1 + reps / 40)
  ) else if (method == "wathan") my_data$temp_maxes <- round(digits = 1,
                                               x = one_RM * (48.8 + 53.8 * exp(-0.075 * reps)) / 100
  ) else stop("Unknown method specified.")

  my_data$temp_maxes <- ifelse(my_data$reps == 1,
                               my_data$one_RM,
                               my_data$temp_maxes)

  return(my_data$temp_maxes)

}


# Training max provides an everyday training maximum commonly used to set percentages for weight programs.
# For a given program, this function will provide a training max for each exercise, equipment, and variant used in the program.
# To do: for training programs where the lifter hasn't used the specified combination of exercise, equipment, and variant, drop the variant.
training_max <- function(weightlifting.log = NA, program = NA, percentage = 0.90, method = "epley", ...) {

  if (! is_valid_weightlifting_log(weightlifting.log)) stop("Please enter a valid weightlifting log.")

  if (! all(! is.na(program), program %in% rweightlifting::available_programs())) {
    stop(paste0("Please provide a valid program. Choices are ", paste0(rweightlifting::available_programs(), collapse = ", "), "."))
  }

  temp.program <- eval(call(program))
  temp.program.schedule <- temp.program$schedule

  if (! all(unique(temp.program.schedule$exercise) %in% unique(weightlifting.log$exercise))) {
    stop(paste0(
      "Your weightlifting log does not contain ",
      paste0(
        unique(temp.program.schedule$exercise)[
          ! unique(temp.program.schedule$exercise) %in% unique(weightlifting.log$exercise)
        ],
        collapse = ", "
      ),
      " exercises, so we cannot generate a 1RM. Please provide a weightlifting log that includes all exercises in the program, which are: ",
      paste0(
        unique(temp.program.schedule$exercise),
        collapse = ", "
      ),
      ".",
      collapse = ""
    ))
  }

  # At this point, we have a valid weightlifting log that contains all required exercises.
  # We need to calculate a recent absolute 1RM for each exercise in the new program, then multiply that by the training max percentage.

  unique_exercise_variants_in_program <- unique(temp.program.schedule[ , c("exercise", "equipment", "variant")])

  lifts_in_program <- weightlifting.log %>%
    inner_join(unique_exercise_variants_in_program, by = c("exercise", "equipment", "variant"))

  est.recent.maxes <- one_rep_max_for_program(lifts_in_program)
  est.recent.maxes$RM.max <- n_rep_max(
    one_RM = est.recent.maxes$roll.max,
    reps = temp.program$RM_reps,
    method = method
  )

  est.recent.maxes %>%
    mutate(training_max = RM.max * percentage) %>%
    select(exercise, equipment, variant, training_max) %>%
    group_by(exercise, equipment, variant) %>%
    summarize(training_max = round(mean(training_max), 1))

}

# This function provides a 1RM for each lift in a program
# For programs that use a different RM to establish a training max, these numbers must be converted
one_rep_max_for_program <- function(lifts_in_program = NULL, method = "epley", ...) {

  if (is.null(lifts_in_program)) {
    stop("Please provide a valid weightlifting log.")
  }

  top.sets <- top_sets(lifts_in_program, method = method)

  est.recent.maxes <- top.sets %>%
    #select(date, exercise, roll.max) %>%
    unique() %>%
    group_by(exercise, variant) %>%
    mutate(roll.max = round(rollapply(est.max, 5, mean, partial = TRUE, align = "left"), 1)) %>%
    group_by(exercise, variant) %>%
    top_n(1, wt = date) %>%
    ungroup()

  est.recent.maxes
}

top_sets <- function(weightlifting.log = NULL, method = NA) {

  if (! is_valid_weightlifting_log(weightlifting.log)) stop("Please enter a valid weightlifting log.")


  temp <- weightlifting.log %>%
    as_tibble() %>%
    select(-set) %>%
    unique() %>%
    mutate(
      epley = one_rep_max(method = "epley", weight = weight, reps = reps),
      brzycki = one_rep_max(method = "brzycki", weight = weight, reps = reps),
      mcglothin = one_rep_max(method = "mcglothin", weight = weight, reps = reps),
      lombardi = one_rep_max(method = "lombardi", weight = weight, reps = reps),
      mayhew = one_rep_max(method = "mayhew", weight = weight, reps = reps),
      oconner = one_rep_max(method = "oconner", weight = weight, reps = reps),
      wathan = one_rep_max(method = "wathan", weight = weight, reps = reps)
    ) %>%
    gather(rep_max_formulas, key = "method", value = "est.max")

  if (! is.na(method) & method %in% rep_max_formulas) {
    temp <- temp %>%
    filter(method == method)
  }

  temp <- temp %>%
    group_by(exercise, date, method) %>%
    filter(reps > 0) %>%
    top_n(1, est.max) %>% # Only best set counts per exercise, per day
    unique() %>%
    ungroup() %>%
    arrange(desc(date)) %>%
    group_by(exercise, method) %>%
    mutate(roll.max = round(rollapply(est.max, 8, max, partial = TRUE, align = "left"), 1)) %>%
    mutate(culled = ifelse(
      est.max >= .9 * roll.max,
      FALSE,
      TRUE
    )) %>%
    #mutate(last.max = lead(est.max), next.max = lag(est.max)) %>%
    ungroup() %>%
    #filter(lift.weight >= 0.85 * last.max | lift.weight >= 0.85 * next.max) # Removing deload weeks
    filter(culled == FALSE) %>% # Removing deload weeks
    select(-culled, -roll.max)

  temp
}
