get.last.glacial.max.time <-  function(x){
  x$data_ROC$data %>% 
    filter(BIN > 8e3 & BIN < 16e3) %>% 
    filter(fit == max(fit)) %>% 
    dplyr::select(BIN) %>% 
    pluck(1) %>% 
    round(.,digits = 0)
}
