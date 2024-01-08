
read_data_catalunya <- function(file) {
  read_delim(
    file,
    delim = ";",
    escape_double = FALSE,
    col_types = cols(...1 = col_skip()),
    locale = locale(
      decimal_mark = ",",
      grouping_mark = "."),
    trim_ws = TRUE)

}

