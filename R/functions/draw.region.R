draw.region <-  function(region,
                         plot_data ,
                         BIN_data,
                         blue_samples = F,
                         RoC_max_value = 1.3,
                         axis_ratio = 2) {
  
  data_w <- 
    plot_data %>%
    filter(REGION == region) %>%
    ungroup()
  
  data_w_sum <- 
    BIN_data %>%
    filter(REGION == region) %>%
    ungroup()
  
  # 95% quantile ROC GAM
  gam_upq <-
    select.model(
      var_y = "ROC_upq",
      var_x = "BIN",
      family = "tw()",
      data = data_w_sum,
      weights = "N_samples"
    )
  
  pred_gam_upq <-  predict.gam.values(gam_model = gam_upq,
                                      var_x = "BIN",
                                      deriv = T)
  
  # PEAK POINTS GAM
  gam_PEAK <-
    select.model(
      var_y = "P_value_mean",
      var_x = "BIN",
      family = "betar(eps=.Machine$double.eps*1e8)",
      data = data_w_sum,
      weights = "N_samples"
    )
  
  pred_gam_PEAK <-  predict.gam.values(gam_model = gam_PEAK,
                                       var_x = "BIN",
                                       deriv = T)
  
  p_fin <- 
    draw.gam.custom(
      data = data_w_sum,
      pred_gam_up = pred_gam_upq,
      pred_gam_pk = pred_gam_PEAK,
      region = region,
      blue_samples = blue_samples,
      siluete = T,
      palette_x = pallete_1,
      deriv = T,
      y_cut = RoC_max_value,
      axis_ratio = axis_ratio
    )
  
  return(
    list(
      plot = p_fin,
      model_ROC = gam_upq,
      data_ROC = pred_gam_upq,
      model_peak = gam_PEAK,
      data_peak = pred_gam_PEAK
    )
  )
}
