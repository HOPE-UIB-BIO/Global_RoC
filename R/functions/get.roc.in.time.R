get.roc.in.time <-  function(x, time){
  x$data_ROC$data %>% 
    slice(which.min(abs(BIN - time))) %>% 
    dplyr::select(fit) %>% 
    pluck(1) %>% 
    round(., digits = 2)
}