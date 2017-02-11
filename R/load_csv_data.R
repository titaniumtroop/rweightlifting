# Expected CSV format:
# date,exercise,variant,set1weight,set1reps,set2weight,set2reps,set...weight,set...reps,setNweight,setNreps
# E.g., 2017-01-26,bench,barbell,102.5,5,135,5,155,5,180,5,210,3,155,13

load_csv_data <- function(files = FALSE, datadir = FALSE) {

  if (is.na(datadir)) {
    stop("You must enter a directory for your weightlifting files.")
  }

  out.file <- data.frame()

  if (files) {
    # Load data from files passed to function (interactive Rshiny)
  } else {
    # Load data from datadir
    file.names <- dir(datadir, pattern = ".csv")

    for (i in 1:length(file.names)){
      file <- read.csv(paste(datadir, "/", file.names[i], sep=""), header=TRUE, sep=",", stringsAsFactors = FALSE)
      file$program <- sub(".\\w+$", "", file.names[i]) # set program to name of file
      file <- file[, c("program", setdiff(names(file), c("program")))] # Move program to first column

      set.info <- grep("set", names(file), ignore.case = TRUE, value = TRUE)
      other.info <- names(file[, ! names(file) %in% set.info])

      # Get max set number
      set.num <- max(gsub("set[_ ]*(\\d+).+", "\\1", set.info), na.rm = TRUE)

      # Loop through sets and add to dataframe
      for (i in seq(1:set.num)) {
        this.set <- grep(paste("set[_ ]*", i, sep = ""), set.info, ignore.case = TRUE, value = TRUE)
        temp <- file[ , c(other.info, this.set)]

        if (grepl("reps", this.set[1], ignore.case = TRUE)) {
          names(temp) <- c(other.info, "reps","weight")
        } else {
          names(temp) <- c(other.info, "weight","reps")
        }
        out.file <- rbind(out.file, temp)
      }
    }
  }
  # Return merged dataframe, removing NA values
  out.file$date <- as.Date(out.file$date)
  out.file[! is.na(out.file$weight) & ! is.na(out.file$reps), ]
}
