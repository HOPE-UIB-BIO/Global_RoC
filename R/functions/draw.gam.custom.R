draw.gam.custom <- function(data,
                     pred_gam_up,
                     pred_gam_pk,
                     region,
                     siluete = F,
                     blue_samples = F,
                     plot_maxima = F,
                     palette_x = pallete_1,
                     deriv = F,
                     y_cut = 1.8,
                     y_start = 0,
                     axis_ratio = 2) {
  p0 <- 
    ggplot() +
    scale_x_continuous(
      trans = "reverse",
      breaks = seq(0, 18e3, 2e3),
      labels = seq(0, 18, 2)
    ) +
    coord_cartesian(xlim = c(age_treshold, 0)) +
    theme_classic() +
    theme(
      legend.position = "none",
      text = element_text(size = text_size),
      panel.border = element_rect(
        fill = NA,
        colour = "gray30",
        size = 0.1
      )
    ) +
    labs(x = "Age (yr BP)")
  
  if ( all(plot_maxima != F)){
    p0 <-
      p0 + 
      geom_segment(
        aes(
          x = plot_maxima[1],
          xend = plot_maxima[1],
          y = plot_maxima[2] + 0.12,
          yend = plot_maxima[2] + 0.05),
        col = "gray30",
        size = 0.2,
        arrow = arrow(length = unit(0.05, "npc"))) +
      geom_segment(
        aes(
          x = plot_maxima[3],
          xend = plot_maxima[3],
          y = plot_maxima[4] + 0.12,
          yend = plot_maxima[4] + 0.05),
        col = "gray30",
        size = 0.2,
        arrow = arrow(length = unit(0.05, "npc")))
  }
  
  
  if (deriv == T) {
    # ROC
    
    p3 <- 
      p0 +
      geom_vline(
        xintercept = seq(0, 18e3, 2e3),
        color = "gray90",
        size = 0.1) +
      geom_ribbon(
        data = pred_gam_up$data,
        aes(
          x = BIN,
          y = fit,
          ymin = lower,
          ymax = upper
        ),
        fill = "gray80",
        size = 0.1,
        alpha = ifelse(pred_gam_up$p < 0.05, 0.5, 0)) +
      geom_line(
        data = pred_gam_up$data,
        aes(x = BIN, y = fit),
        lty = ifelse(pred_gam_up$p < 0.05, 1, 2),
        size = 0.1) +
      scale_fill_manual(values = palette_x) +
      scale_color_manual(values = palette_x) +
      labs(y = "ROC Score, 95% quantile") +
      scale_y_continuous(
        limits = c(y_start, y_cut),
        breaks = seq(0, y_cut, 0.1),
        sec.axis =  sec_axis(
          name =  ,
          ~ (.  - y_start) / axis_ratio ,
          breaks = seq(0, y_cut, 0.1)
        )
      )
    
    
    if (all(is.na(pred_gam_up$first_deriv) == F)) {
      rect_df_upq <-
        left_join(pred_gam_up$first_deriv, pred_gam_up$data, by = "BIN") %>%
        filter(significante_change == T)
      
      if (nrow(rect_df_upq) > 0) {
        p3 <-
          p3 + 
          geom_point(
            data = rect_df_upq,
            aes(x = BIN, y = fit),
            size = 0.1,
            color = "gray30",
            shape = 8
          )
      }
    }
    
    p3 <-
      p3 + 
      geom_point(
        data = data,
        aes(x = BIN, y = ROC_upq, color = region),
        shape = 15,
        size = 1,
        alpha = 1 / 2
      )
    
    # peak point
    
    p3 <- 
      p3 +
      geom_ribbon(
        data = pred_gam_pk$data,
        aes(
          x = BIN,
          y = (fit  * axis_ratio ) + y_start,
          ymin = (lower  * axis_ratio ) + y_start,
          ymax = (upper  * axis_ratio ) + y_start
        ),
        fill = "gray80",
        size = 0.1,
        alpha = ifelse(pred_gam_pk$p < 0.05, 0.5, 0)) +
      geom_line(
        data = pred_gam_pk$data,
        aes(x = BIN, y = (fit  * axis_ratio) + y_start ),
        lty = ifelse(pred_gam_pk$p < 0.05, 3, 2),
        size = 0.1
      )
    
    
    if (all(is.na(pred_gam_pk$first_deriv) == F)) {
      rect_df_pk <-
        left_join(pred_gam_pk$first_deriv, pred_gam_pk$data, by = "BIN") %>%
        filter(significante_change == T)
      
      if (nrow(rect_df_pk) > 0) {
        p3 <-
          p3 + 
          geom_point(
            data = rect_df_pk,
            aes(x = BIN, y = (fit  * axis_ratio) + y_start),
            size = 0.1,
            color = "gray30",
            shape = 8
          )
      }
    }
    
    p3 <-
      p3 + 
      geom_point(
        data = data,
        aes(
          x = BIN,
          y = (P_value_mean  * axis_ratio) + y_start,
          color = region
        ),
        shape = 0,
        size = 1,
        alpha = 1 /2
      )
    
    
  } else {
    p3 <- 
      p0 +
      geom_ribbon(
        data = pred_gam_up$data,
        aes(
          x = BIN,
          y = fit,
          ymin = lower,
          ymax = upper
        ),
        fill = "gray80",
        alpha = ifelse(pred_gam_up$p < 0.05, 0.5, 0)) +
      geom_point(data = data,
                 aes(x = BIN, y = ROC_upq, color = region),
                 shape = 15) +
      geom_line(
        data = pred_gam_up$data,
        aes(x = BIN, y = fit),
        lty = ifelse(pred_gam_up$p < 0.05, 1, 2)) +
      geom_ribbon(
        data = pred_gam_pk$data,
        aes(
          x = BIN,
          y =( fit * axis_ratio) + y_start,
          ymin = (lower * axis_ratio) + y_start,
          ymax = (upper * axis_ratio) + y_start
        ),
        fill = "gray80",
        alpha = ifelse(pred_gam_pk$p < 0.05, 0.5, 0)) +
      geom_point(data = data,
                 aes(x = BIN, y = P_value_mean * 2, color = region),
                 shape = 0) +
      geom_line(
        data = pred_gam_pk$data,
        aes(x = BIN, y = fit * 2),
        lty = ifelse(pred_gam_pk$p < 0.05, 3, 2)) +
      scale_color_manual(values = palette_x) +
      labs(y = "ROC Score, 95% quantile") +
      scale_y_continuous(
        limits = c(y_start, y_cut),
        breaks = seq(0, y_cut, 0.1),
        sec.axis =  sec_axis(
          name =  ,
          ~ (. - y_start) / axis_ratio ,
          breaks = seq(0, y_cut, 0.1)
        )
      )
  }
  
  if (siluete == T) {
    region_coord_w <- 
      region_coord %>%
      filter(REGION == region)
    
    p2 <- 
      ggplot() +
      borders(fill = pallete_1[names(pallete_1) == region],
              colour = NA,
              alpha = .3) +
      coord_fixed(
        xlim = c(region_coord_w$long_min, region_coord_w$long_max),
        ylim = c(region_coord_w$lat_min, region_coord_w$lat_max)) +
      theme_transparent()
    
    p2_g <- ggplotGrob(p2)
    
    p3_a <-
      p3 + 
      rremove("xylab") + 
      annotation_custom(
        grob = p2_g,
        xmin = -1.5e3,
        xmax = -5e3,
        ymin = 0.4,
        ymax = y_cut
      )
    p3_a
    
  } else {
    p3_a <- p3 + rremove("xylab")
  }
  
  if (blue_samples == T) {
    Max_samples <- max(data$N_samples)
    
    p1 <- 
      ggplot() +
      coord_cartesian(xlim = c(age_treshold, 0)) +
      theme_classic() +
      labs(x = "Age (yr BP)",
           y = "Number of samples in each time bin") +
      geom_ribbon(
        data = data,
        aes(x = BIN, ymax = N_samples),
        ymin = 0,
        fill = "lightblue1") +
      scale_x_continuous(trans = "reverse") +
      scale_y_continuous(
        #position = "right",
        breaks = c(0, Max_samples)) +
      theme(axis.title.x = element_blank(),
            text = element_text(size = text_size))
    
    p4 <-
      ggarrange(
        p3_a,
        p1 + 
          rremove("x.text") + 
          rremove("xy.title") + 
          rremove("x.ticks"),
        align = "v",
        nrow = 2,
        heights = c(1, 0.5)
      )
    
  } else {
    p4 <- p3_a + theme(text = element_text(size = text_size))
  }
  
  return(p4)
}
