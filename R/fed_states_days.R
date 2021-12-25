#' Title
#'
#' @param days
#'
#' @return
#'
#'
#' @examples
federal_states_days <- function(days) {
  states <- c("Wien",
              "Niederösterreich",
              "Oberösterreich",
              "Burgenland",
              "Salzburg",
              "Tirol",
              "Steiermark",
              "Kärnten",
              "Vorarlberg")

  expand.grid(days, states) %>%
    dplyr::rename(date = Var1, level = Var2)
}
