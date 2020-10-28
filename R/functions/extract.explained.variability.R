extract.explained.variability <- function(x){
  m_sum <- summary(x)
  m_sum$dev.expl %>%
    round(.,2) %>%
    return()
}
