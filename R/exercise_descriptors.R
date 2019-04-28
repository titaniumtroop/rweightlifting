exercise_descriptors <- function () {

  exercises <- data.frame(
    exercise_major_group = factor(levels = c(
      "back", "lower body", "upper body", "core"
    ), ordered = FALSE), # back, lower body, upper body
    muscle_major_group = factor(levels = c( # muscle groups (in order of involvement)
      "lats", "shoulders", "trapezius", "latissimus dorsi", "biceps", "triceps", "pectoralis major"
    ), ordered = FALSE),
    muscle_minor_groups = factor(levels = NA), # space-separated list of numbers corresponding to a factor level of major_muscle_group, will need a parse/spread function
    exercise_major_type = factor(levels = c(
      "deadlift", "squat", "bench press", "press", "row", "shrug", "pullup", "dip", "tricep extension", "calf raise", "hyperextension", "face pull", "lat pulldown", "bicep curl", "fly", "shoulder rotation", "shoulder raise", "leg raise", "leg extension"
    ), ordered = TRUE), # squat, bench press, press
    exercise_equipment = factor(levels = c(
      "barbell", "hex bar", "EZ bar", "dumbbell", "kettlebell", "plate", "machine", "chains", "body weight", "pullup", "chinup", "bands", NA
    ), ordered = TRUE), # barbell, dumbbell, etc.
    exercise_variant = factor(levels = c(
      "front", "upper", "lower", "left", "right", "overhead", "incline", "decline", "flat", "chains", "low bar", "high bar", "hack", "box", "reverse", "seated", "standing", "conventional", "sumo", "bent over", "pendlay", "kroc", "close", "wide", "preacher", "pronated", "supinated", "combination", "assisted", NA
    ), ordered = FALSE),
    exercise_degree = factor(levels = c(
      NA, "15%", "%30%", "45%", "%60", "75%", "90", "0%", "-15%", "-%30%", "-45%", "-%60", "-75%", "-90%"
    ), ordered = TRUE)
  )

  # exercise_library <- tidyr::complete(exercises, exercise_major_type, exercise_implement)
  # exercise_library[exercise_library$exercise_major_type %in% c("squat", "deadlift", "calf raise", "leg extension"), ]$exercise_major_group <- "lower body"
  # exercise_library[exercise_library$exercise_major_type %in% c("bench press", "dip", "tricep extension", "press", "bicep curl", "fly", "shoulder rotation", "shoulder raise"), ]$exercise_major_group <- "upper body"
  # exercise_library[exercise_library$exercise_major_type %in% c("row", "shrug", "pullup", "face pull", "lat pulldown"), ]$exercise_major_group <- "back"
  # exercise_library[exercise_library$exercise_major_type %in% c("hyperextension", "leg raise"), ]$exercise_major_group <- "core"
  #
  # exercise_library <- exercise_library %>%
  #   mutate_if(is.factor, as.character) %>%
  #   full_join(.,
  #     tibble(
  #       exercise_major_type = c("squat"),
  #       exercise_variant = c("low bar", "high bar", "hack", "box", "front")
  #     ),
  #     by = c("exercise_major_type", "exercise_variant")
  #   )

}
