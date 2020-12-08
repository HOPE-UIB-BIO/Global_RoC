extract.first.increase <- function(x, nested = T) {
  
  if (nested == T){
    x <- x$data_ROC  
  }
  
  y <- 
    x$first_deriv %>% 
    filter(BIN <= 10e3)
    
  first_sig <-
    y %>%
    filter(significante_change == T) %>%
    filter(d_est < 0) %>% 
    dplyr::select(BIN) %>%
    min()
  
  age <-
    y %>%
    mutate(CHANGE = c(diff(significante_change), 0)) %>%
    filter(BIN > first_sig) %>%
    filter(CHANGE != 0) %>%
    dplyr::select(BIN) %>%
    min()
  
  age_round = round(age / 100) * 100
  
  return(age_round)
}