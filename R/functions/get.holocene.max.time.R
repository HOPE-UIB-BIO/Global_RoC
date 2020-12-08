get.holocene.max.time <-  function(x){
  x$data_ROC$data %>% 
    filter(BIN < 2e3) %>% 
    filter(fit == max(fit)) %>% 
    dplyr::select(BIN) %>% 
    pluck(1) %>% 
    round(.,digits = 0)
}

