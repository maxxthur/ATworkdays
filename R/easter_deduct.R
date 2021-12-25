#' Title
#'
#' @param eastern
#'
#' @return
#'
#'
#' @examples
easter_deduct_federal <- function(eastern) {
  easter_m <- eastern + 1
  easter_f <- eastern - 2
  ch <- eastern + 39
  pm <- eastern + 50
  fl <- eastern + 60

  data.frame(date = c(easter_m, easter_f, ch, pm, fl),
             level = "federal")
}


