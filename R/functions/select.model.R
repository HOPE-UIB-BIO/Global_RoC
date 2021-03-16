# get the best k
select.model <-   function(
  var_y,
  var_x,
  family,
  data,
  weights = NA) {
  
  for (i in  seq(from = 8,to = min(c(nrow(data) - 1, 100)), by = 4) )    {
    
    print(paste("trying k=", i))
    
    formula_w <- paste(var_y, "~ s(", var_x,", k=", i,",bs='tp')") #
    
    if (is.na(weights) == F) {
      data <-
        data %>%
        mutate(W = with(data, get(weights)))
      
    data$W <- (data$W / mean(data$W))+1
    #data$W <- scales::rescale(data$W, to=c(1,10))
    #data$W <- log(data$W+1)  
    
    } else {
      data <-
        data %>%
        mutate(W = rep(1, nrow(data)))
    }
    
    suppressWarnings(
      gam_w <-
        gam(
          formula = as.formula(formula_w),
          data = data,
          family =  noquote(family),
          weights = W,
          method = "REML",
          niterPQL = 50
        )
    )
    
    # save the result from the k.check fc
    f <- function(b,
                  k.sample = 10e3,
                  k.rep = 1e3) {
      mgcv:::k.check(b, subsample = k.sample, n.rep = k.rep)
    }
    
    suppressWarnings(basis <- f(gam_w))
    
    if (is.na(basis[4]) == F) {
      if (basis[4] > 0.05) {
        break
      }
    }
    
  }
  return(gam_w)
}
