extract.first.increase <- function(x) {
  
  first_sig <-
    x$first_deriv %>%
    filter(significante_change == T) %>%
    dplyr::select(BIN) %>%
    min()
  
  age <-
    x$first_deriv %>%
    mutate(CHANGE = c(diff(significante_change), 0)) %>%
    filter(BIN > first_sig) %>%
    filter(CHANGE != 0) %>%
    dplyr::select(BIN) %>%
    min()
  
  age_round = round(age / 100) * 100
  
  return(age_round)
}