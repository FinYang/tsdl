library(tidyverse)

tsdl <- read_csv("tsdl.csv") %>%
  mutate(
    Subject = recode(Subject, `Finan+H77ce` = "Finance"),
    Description = str_replace_all(Description, "Biomonthly", "Bimonthly"),
    Description = strsplit(Description, "\n"),
    Frequency = replace_na(Frequency, 1),
    Start = replace_na(Start, 1),
    Skip = replace_na(Skip, 0)
  )

read.tsdl <- function(i, silent = TRUE) {
  series <- as.data.frame(tsdl[i,])
  fn <- paste0(series[,"Folder"], "/", series[,"File"])
  # print(fn)

  if (series[,"File"] == "niagra.dat") {
    x <- scan(fn, skip = series[,"Skip"])
    x <- x[x > 2000]
  }
  else if (series[,"Format"] == "Column") {
    if (class(try(x <- read.table(fn, skip = series[,"Skip"], header = TRUE, colClasses = "character", flush = TRUE, fill = TRUE), silent = silent)) == "try-error") {
      if (class(try(x <- read.csv(fn, sep = ",", skip = series[,"Skip"], header = TRUE, colClasses = "character", flush = TRUE, fill = TRUE), silent = silent)) == "try-error") {
        stop(paste("Problem with series", fn))
      }
    }
    if (ncol(x) == 1) {
      if (class(try(x <- read.table(fn, sep = ",", skip = series[,"Skip"], header = TRUE, colClasses = "character", flush = TRUE, fill = TRUE), silent = silent)) == "try-error") {
        stop(paste("Problem with series", fn))
      }
    }
    # Remove columns that are dates, time stamps or missing
    drop <- numeric(0)
    for (j in seq(ncol(x))) {
      y <- as.numeric(gsub(",", "", x[, j]))
      if (sum(is.na(y)) == length(y)) {
        vy <- NA
      } else if (sum(is.na(diff(y))) == length(y) - 1) {
        vy <- NA
      } else {
        vy <- var(y, na.rm = TRUE)
      }
      if (is.na(vy)) {
        drop <- c(drop, j)
      } else if (vy < 1e-10) {
        drop <- c(drop, j)
      } else if (var(diff(y), na.rm = TRUE) < 1e-10) {
        drop <- c(drop, j)
      } else if (is.element(colnames(x)[j], c("year", "Year", "year.", "yr", "Yrwk"))) {
        drop <- c(drop, j)
      } else {
        x[, j] <- y
      }
    }
    if (length(drop) > 0) {
      x <- x[, -drop, drop = FALSE]
    }
  }
  else {
    x <- scan(fn, skip = series[,"Skip"], na.strings = c("NA", "-", "*", "999999", "000"))
  }
  # Add time series characteristics
  x <- ts(x, frequency = series[,"Frequency"], start = series[,"Start"])
  attributes(x) <- c(attributes(x),
                     source = series[, "Source"],
                     description = series[, "Description"],
                     subject = series[, "Subject"])
  return(x)
}

tsdl_data <- list()
for (i in seq(nrow(tsdl))) {
  tsdl_data[[i]] <- read.tsdl(i)
}

meta_tsdl <- tsdl %>%
  select(Source, Description, Frequency, Start, Subject) %>%
  rename_all(tolower)

tsdl <- tsdl_data
class(tsdl) <- "tsdl"

usethis::use_data(tsdl, overwrite = TRUE)
usethis::use_data(meta_tsdl, overwrite = TRUE)
