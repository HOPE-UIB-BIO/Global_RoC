extract.explained.variability <- function(x, nested = T){
  
  if (nested == T){
    x <- x$model_ROC  
  }
  
  
  m_sum <- summary(x)
  m_sum$dev.expl %>%
    round(.,2) %>%
    return()
}
