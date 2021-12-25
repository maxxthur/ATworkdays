#' Title
#'
#' @return
#'
#'
#' @examples
fixed_federal_holidays <- function(Y) {

  days <- c("01-01",
            "06-01",
            "01-05",
            "15-08",
            "26-10",
            "01-11",
            "08-12",
            "25-12",
            "26-12")

  dates <- expand.grid(days, Y) %>%
    dplyr::mutate(date = lubridate::dmy(paste(Var1, Var2, sep = "-"))) %>%
    .$date

  data.frame(date = dates, level = "federal")
}
