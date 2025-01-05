.onAttach <- function(libname, pkgname) {
  packageStartupMessage("
Welcome to the DF Package!
---------------------------
DF stands for Debora and Flavio.

This package includes a single function: time_since()

Be sure to check the description file for more details!
")
}
#' Time since...
#'
#' @return Message!
#' @export
time_since <- function() {
  # Predefined initial dates
  civil <- as.Date("2022-10-27")  # Civil marriage
  cerimonia <- as.Date("2023-01-05")  # Wedding ceremony
  together_since <- as.Date("2014-03-24")  # Dating

  # Get the current date
  current_date <- Sys.Date()

  # Calculate the difference in days for both dates
  days_passed_civil <- as.numeric(current_date - civil)
  days_passed_cerimonia <- as.numeric(current_date - cerimonia)
  days_passed_together <- as.numeric(current_date - together_since)  # Days since together

  # Function to calculate years, months, and days
  calculate_time <- function(days_passed) {
    years <- days_passed %/% 365
    months <- (days_passed %% 365) %/% 30
    days <- (days_passed %% 365) %% 30
    return(list(years = years, months = months, days = days))
  }

  # Calculate time since both dates
  time_since_civil <- calculate_time(days_passed_civil)
  time_since_cerimonia <- calculate_time(days_passed_cerimonia)
  time_since_together <- calculate_time(days_passed_together)  # Time since together

  # Create a message to show the elapsed time
  time <- paste0("Debora and Flavio have been officially married for ",
                         time_since_civil$years, " years, ",
                         time_since_civil$months, " months, and ",
                         time_since_civil$days, " days, but ",
                         time_since_cerimonia$years, " years, ",
                         time_since_cerimonia$months, " months, and ",
                         time_since_cerimonia$days, " days have passed since their ceremony with family. ",
                         "They are together for ",
                         time_since_together$years, " years, ",
                         time_since_together$months, " months, and ",
                         time_since_together$days, " days.")

  return(time)
}
