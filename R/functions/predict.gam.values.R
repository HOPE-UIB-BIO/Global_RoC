predict.gam.values <- function(gam_model, var_x, deriv = F) {
  
  new_data  <-
    tibble(VAR = seq(
      gam_model$var.summary[[1]][1],
      gam_model$var.summary[[1]][3],
      length.out = 100
    ))
  
  names(new_data) <- var_x
  
  new_data_pred <- 
    cbind(new_data,
          data.frame(
            predict(
              gam_model,
              se.fit = TRUE,
              newdata = new_data,
              type = "response"
            ))) %>%
    as_tibble()
  
  crit_t <- qt(0.975, df = df.residual(gam_model))
  
  new_data_pred <- 
    new_data_pred %>%
    mutate(upper = fit + (crit_t * se.fit),
           lower = fit - (crit_t * se.fit))
  
  k_value <-
    gam_model$formula %>%
    as.character(.) %>%
    .[3] %>%
    str_extract(., "k = [:digit:]*") %>%
    str_replace(., "k = ", "") %>%
    as.double(.)
  
  gam_model_summary <-
    gam_model %>%
    summary()
  
  p_value <-  gam_model_summary$s.table[4]
  
  if (deriv == T & p_value < 0.05) {
    
    gam_model_deriv <- fderiv(gam_model, newdata = new_data, n = 1000)
    
    gam_model_deriv_sint <-
      with(new_data, cbind(
        confint(
          gam_model_deriv,
          nsim = 1000,
          type = "simultaneous",
          transform	= T
        ),
        BIN = BIN
      )) %>% 
      as_tibble()
    
    deriv <-
      gam_model_deriv_sint %>%
      mutate(significante_change = lower > 0 | upper < 0) %>%
      dplyr::select(-term) %>%
      rename(d_lower = lower,
             d_est = est,
             d_upper = upper)
  } else {
    deriv <- NA
  }
  
  return(
    list(
      data = new_data_pred,
      k = k_value,
      p = p_value,
      first_deriv = deriv
    ))
}
