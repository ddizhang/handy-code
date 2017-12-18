
#' conditional mutate
#' 
#' @example
#' data %>% mutate_cond(is.na(quote_2013)|quote_2013 < quantity_2013, quote_2013 = quantity_2013) 

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

