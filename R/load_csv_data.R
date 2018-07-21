# Expected CSV format:
# date,exercise,variant,set1weight,set1reps,set2weight,set2reps,set...weight,set...reps,setNweight,setNreps
# E.g., 2017-01-26,bench,barbell,102.5,5,135,5,155,5,180,5,210,3,155,13

load_csv_data <- function(files = FALSE, datadir = FALSE, header = TRUE) {

  if (is.na(datadir)) {
    stop("You must enter a directory for your weightlifting files.")
  }

  out.file <- data.frame()

  if (files) {
    # Load data from files passed to function (interactive Rshiny)
  } else {
    # Load data from datadir
    file.names <- dir(datadir, pattern = ".csv")

    for (i in 1:length(file.names)) {

      file <- read.csv(
        paste(datadir, "/", file.names[i], sep=""),
        header = header,
        sep = ",",
        stringsAsFactors = FALSE,
        strip.white = TRUE
      )

      file$program <- sub(".\\w+$", "", file.names[i]) # set program to name of file
      file <- file[, c("program", setdiff(names(file), c("program")))] # Move program to first column

      if (requireNamespace("tidyr", quietly = TRUE) &
          requireNamespace("dplyr", quietly = TRUE) &
          requireNamespace("readr", quietly = TRUE)) {
        col.classes <- sapply(file, class)
        numeric.cols <- names(col.classes[col.classes == "numeric" | col.classes == "integer"])
        logical.cols <- names(col.classes[col.classes == "logical"])

        if (all(is.na(file[, logical.cols]))) {
          file <- dplyr::select(file, -logical.cols)
        }

        file <- tidyr::gather(file, "key", "value", numeric.cols, na.rm = TRUE)
        file <- dplyr::mutate(file, set = readr::parse_number(key))
        file <- dplyr::mutate(file, key = ifelse(grepl("rep", key), "reps", "weight"))
        file <- tidyr::spread(file, key, value)
        out.file <- dplyr::bind_rows(out.file, file)
      }

      else {
        requireNamespace("tidyr", quietly = F)
        requireNamespace("dplyr", quietly = F)
        requireNamespace("readr", quietly = F)
        set.cols <- grep("set", names(file), ignore.case = TRUE, value = TRUE)
        other.cols <- names(file[, ! names(file) %in% set.cols])

        # Get max set number
        set.num <- max(as.numeric(sub("set[_ ]*(\\d+).+", "\\1", set.cols)), na.rm = TRUE)

        # Loop through sets and add to dataframe
        for (j in seq(1:set.num)) {
          this.set <- grep(paste("set[_ ]*", j, "", sep = ""), set.cols, ignore.case = TRUE, value = TRUE)[1:2]
          temp <- file[ , c(other.cols, this.set)]

          if (grepl("reps", this.set[1], ignore.case = TRUE)) {
            names(temp) <- c(other.cols, "reps","weight")
          } else {
            names(temp) <- c(other.cols, "weight","reps")
          }

          temp$set <- j
          out.file <- rbind(out.file, temp)
        }
      }
    }
  }
  # Return merged dataframe, removing NA values
  out.file$date <- as.Date(out.file$date)
  out.file[! is.na(out.file$weight) & ! is.na(out.file$reps), ]
}
