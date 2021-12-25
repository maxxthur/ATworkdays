#' Title
#'
#' @param Y
#'
#' @return
#'
#'
#' @examples
fixed_state_holidays <- function(Y) {
  days <- c(rep("19-03", 4),
            "04-05",
            "24-09",
            "10-10",
            "11-11",
            rep("15-11", 2))

  dates <- expand.grid(days, Y) %>%
    dplyr::mutate(date = lubridate::dmy(paste(Var1, Var2, sep = "-"))) %>%
    .$date

  level <- c("Kärnten",
             "Steiermark",
             "Tirol",
             "Vorarlberg",
             "Oberösterreich",
             "Steiermark",
             "Kärnten",
             "Burgenland",
             "Niederösterreich",
             "Wien")


  data.frame(date = dates,
             level = rep(level, length(Y)))
}
