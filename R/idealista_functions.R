
read_data_catalunya <- function(file) {
  read_csv(
    file,
    col_types = cols(...1 = col_skip(), period = col_date(format = "%Y-%m-%d"))
  ) |>
    filter(province %in%
      c("Barcelona","Girona","Lleida","Tarragona")
    )
}





