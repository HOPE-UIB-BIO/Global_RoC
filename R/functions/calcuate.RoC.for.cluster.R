calcuate.RoC.for.cluster <- function(data_w, bin_size = time_bin, ROC_metric = "ROC_MAIN" ){ 
  
  
  
  getPalette <-
    colorRampPalette(brewer.pal(min(8, length(
      levels(data_w$cluster)
    )), "Set2"))
  
  palette_w <- getPalette(length(levels(data_w$cluster)))
  names(palette_w) <- levels(data_w$cluster)
  
  long_lim <-  c(min(data_w$long), max(data_w$long))
  lat_lim  <-  c(min(data_w$lat), max(data_w$lat))
  
  p1 <- 
    data_w %>%
    ggplot(aes(x = long, y = lat)) +
    borders(fill = "gray90", colour = "gray90") +
    coord_quickmap(xlim = long_lim, ylim = lat_lim) +
    geom_point(aes(color = cluster), size = 3) +
    theme_classic() +
    scale_color_manual(values = palette_w) +
    labs(x = "lattitude",
         y = "longitude") +
    theme(legend.position = "none")
  
  
  data_w_ROC <- 
    data_w %>%
    unnest(all_of(ROC_metric)) %>%  
    mutate(BIN =  ceiling(Age / bin_size) * bin_size) %>%
    dplyr::select(REGION, cluster, BIN, ROC) %>%
    group_by(REGION, cluster, BIN) %>%
    summarise(
      .groups = "drop",
      N = n(),
      ROC_m = median(ROC),
      ROC_sd = sd(ROC),
      ROC_up = quantile(ROC, 0.975),
      ROC_dw = quantile(ROC, 0.025),
      ROC_se = ROC_sd / sqrt(N)) %>%
    left_join(
      .,
      data_w %>%
        arrange(dataset.id) %>%
        unnest(all_of(ROC_metric)) %>%
        mutate(BIN =  ceiling(Age / bin_size) * bin_size) %>%
        dplyr::select(REGION, cluster, dataset.id, BIN, Peak) %>%
        ungroup() %>%
        group_by(REGION, cluster, dataset.id, BIN) %>%
        dplyr::summarise(.groups = "keep",
                         Peak_m = max(Peak)) %>%
        group_by(REGION, cluster, BIN) %>%
        dplyr::summarise(
          .groups = "keep",
          N = n(),
          P_value_mean = mean(Peak_m),
          P_value_sd = sd(Peak_m),
          P_value_se = P_value_sd / sqrt(N)
        ),
      by = c("REGION", "cluster", "BIN")) %>%
    mutate(
      P_value_mean = replace(P_value_mean, is.na(P_value_mean), 0),
      P_value_sd = replace(P_value_sd, is.na(P_value_sd), 0)
    )
  
  cluster_result <-
    data_w %>%
    group_by(cluster) %>%
    summarise(.groups = "drop",
              N = n()) %>%
    dplyr::filter(N >= 10) %>%
    mutate(C_char = as.character(cluster)) %>%
    mutate(data =  purrr::map(
      C_char,
      .f = function(x) {
        data_w_ROC_c <- 
          data_w_ROC  %>%
          dplyr::filter(cluster == as.character(x)) %>%
          rename(N_samples = N.x,
                 ROC_upq = ROC_up)
        return(data_w_ROC_c)
      })) %>%
    mutate(gam_ROC_upq = purrr::map(
      data,
      .f = function(x) {
        gam_ROC_upq <-
          select.model(
            var_y = "ROC_upq",
            var_x = "BIN",
            family = "tw()",
            data = x,
            weights = "N_samples"
          )
        return(gam_ROC_upq)
      })) %>%
    mutate(pred_gam_ROC_upq = purrr::map(
      gam_ROC_upq,
      .f = function(x) {
        pred_gam_ROC_upq <- predict.gam.values(x, "BIN", deriv = T)
        return(pred_gam_ROC_upq)
      })) %>%
    mutate(gam_ROC_Peak = purrr::map(
      data,
      .f = function(x) {
        gam_ROC_Peak <-
          select.model(
            var_y = "P_value_mean",
            var_x = "BIN",
            family = "betar(eps=.Machine$double.eps*1e8)",
            data = x,
            weights = "N_samples"
          )
        return(gam_ROC_Peak)
      })) %>%
    mutate(pred_gam_ROC_Peak = purrr::map(
      gam_ROC_Peak,
      .f = function(x) {
        pred_gam_ROC_Peak <- predict.gam.values(x, "BIN", deriv = T)
        return(pred_gam_ROC_Peak)
      })) %>%
    mutate(Plot = purrr::pmap(
      list(data, pred_gam_ROC_upq, pred_gam_ROC_Peak, C_char),
      .f = function(x, y, z, u) {
        plot_res = draw.gam.custom(
          x, y, z,
          region = as.character(u),
          siluete = F,
          palette_x = palette_w,
          deriv = T,
          y_cut = 1.75
        )
        return(plot_res)
      }
    ))
  
  p2 <- 
    ggarrange(plotlist = cluster_result$Plot) %>%
    annotate_figure(
      bottom = text_grob("Age (ka) ", size = text_size),
      left = text_grob("ROC Score, 95% quantile", size = text_size, rot = 90),
      right = text_grob(
        " Proportion of peak points",
        size = text_size,
        rot = -90
      )
    )
  
  p_fin <- ggarrange(p1, p2, nrow = 1, widths = c(1, .75))
  
  return(
    list(
      data_sites = data_w,
      data_c = cluster_result,
      plot = p_fin
    ))
  
}


