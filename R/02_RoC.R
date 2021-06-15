#----------------------------------------------------------#
#
#
#                 Global Rate-of-Change patterns
#
#                   RoC continental patterns
#
#                     Ondrej Mottl - 2020
#
#----------------------------------------------------------#

source("R/00_config.R")

#----------------------------------------------------------#
# 1. Prepare dataset -----
#----------------------------------------------------------#

Data_RoC <- 
  Dataset_work %>%
  unnest(cols = c(ROC_MAIN)) %>%
  mutate(BIN = ceiling(Age / time_bin) * time_bin) %>%
  dplyr::select(REGION, dataset.id, long, lat, elev, BIN, Age, ROC, Peak) 

Data_RoC_sum <- 
  Data_RoC %>%
  group_by(REGION, BIN) %>%
  dplyr::summarise(
    .groups = "keep",
    N_samples = n(),
    ROC_mean =  mean(ROC),
    ROC_median = median(ROC),
    ROC_upq = quantile(ROC, 0.95),
    ROC_sd = sd(ROC) ) %>%
  left_join(
    .,
    Data_RoC %>%
      arrange(dataset.id) %>%
      dplyr::select(REGION, dataset.id, BIN, Peak) %>%
      ungroup() %>%
      group_by(REGION, dataset.id, BIN) %>%
      dplyr::summarise(.groups = "keep",
                       Peak_m = max(Peak)) %>%
      group_by(REGION, BIN) %>%
      dplyr::summarise(
        .groups = "keep",
        N = n(),
        P_value_mean = mean(Peak_m),
        P_value_sd = sd(Peak_m),
        P_value_se = P_value_sd / sqrt(N) ),
    by = c("REGION", "BIN") ) %>%
  mutate(
    P_value_mean = replace(P_value_mean, is.na(P_value_mean), 0),
    P_value_sd = replace(P_value_sd, is.na(P_value_sd), 0) )


#----------------------------------------------------------#
# 2. RoC per continent -----
#----------------------------------------------------------#

P_Africa <- 
  draw.region(
    region = "Africa",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_Asia <- 
  draw.region(
    region = "Asia",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_Europe <-
  draw.region(
    region = "Europe",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_Latin_America <- 
  draw.region(
    region = "Latin America",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_North_America <- 
  draw.region(
    region = "North America",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_Oceania <- 
  draw.region(
    region = "Oceania",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)



#----------------------------------------------------------#
# 2.2 Estimate RoC values per continent -----
#----------------------------------------------------------#


region_ROC_table <-
  tibble(
    REGION = names(pallete_1)) %>% 
  mutate(
    plot_name = str_replace(
      paste0("P_",REGION),
      " ",
      "_")) %>% 
  mutate(
    
    start_of_increase = purrr::map_dbl(
      plot_name, 
      possibly(
        function(x){extract.first.increase(get(x))},
        NA_real_)),
    
    Mod_explained = purrr::map_dbl(
      plot_name, 
      function(x){extract.explained.variability(get(x))}),
    
    holocene_max_time = purrr::map_dbl(
      plot_name, 
      function(x){get.holocene.max.time(get(x))}),
    
    holocene_max_roc = purrr::map2_dbl(
      plot_name, 
      holocene_max_time,
      function(x, y){get.roc.in.time(get(x), y)}),
    
    last_glacial_max_time = purrr::map_dbl(
      plot_name, 
      function(x){get.last.glacial.max.time(get(x))}),
    
    last_glacial_max_roc = purrr::map2_dbl(
      plot_name, 
      last_glacial_max_time,
      function(x,y){get.roc.in.time(get(x),y)}),
    
    roc_percentage_increase_in_holocene = round(
      ( (holocene_max_roc - last_glacial_max_roc   ) / holocene_max_roc  ) * 100,
      digits = 2)) %>% 
  dplyr::select(-c(plot_name)) 


write.csv(region_ROC_table,"DATA/output/region_ROC_table.csv")

#----------------------------------------------------------#
# 2.3. Figure 02 : Roc per region ----- 
#----------------------------------------------------------#

Regional_curves_fin <-
region_ROC_table %>% 
  mutate(
    final_plot = purrr::pmap(
      list(
        REGION,
        holocene_max_time,
        holocene_max_roc, 
        last_glacial_max_time,
        last_glacial_max_roc),
      .f = function(region, hol_time, hol_roc, glac_time, glac_roc){
        
        original_plot <-
          draw.region(
            region = region,
            plot_data = Data_RoC ,
            BIN_data = Data_RoC_sum)
        
        data_w_sum <- 
          Data_RoC_sum %>%
          filter(REGION == region) %>%
          ungroup()
        
        draw.gam.custom(
          data = data_w_sum,
          pred_gam_up = original_plot$data_ROC,
          pred_gam_pk = original_plot$data_peak,
          region = region,
          blue_samples = F,
          siluete = T,
          plot_maxima = c(hol_time, hol_roc, glac_time, glac_roc),
          palette_x = pallete_1,
          deriv = T,
          y_cut = 0.95, # 1.3  / 0.95
          y_start = 0.32, # 0 / 0.32 
          axis_ratio = 1.1 # 2 / 1.1
        ) %>% 
          return()})) %>% 
  dplyr::select(REGION, final_plot)


FIGURE_02 <- 
  ggarrange(
    Regional_curves_fin$final_plot[[1]] + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) + 
      rremove("xylab") +
      rremove("x.text") + 
      rremove("x.ticks"),
    Regional_curves_fin$final_plot[[3]] + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") +
      rremove("x.text") +
      rremove("x.ticks"),
    Regional_curves_fin$final_plot[[5]] +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") +
      rremove("x.text") +
      rremove("x.ticks"),
    Regional_curves_fin$final_plot[[2]] +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    Regional_curves_fin$final_plot[[4]] +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    Regional_curves_fin$final_plot[[6]] +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    ncol = 3,
    nrow = 2,
    labels = c("A", "B", "C", "D", "E", "F"),
    font.label = list(size = text_size) ) %>%
  annotate_figure(
    bottom = text_grob("Age (ka) ", size = text_size),
    left = text_grob("RoC Score, 95% quantile", size = text_size, rot = 90),
    right = text_grob(" Proportion of peak points", size = text_size, rot = -90) )

ggsave(
  "figures/FIGURE_02.pdf",
  FIGURE_02,
  width = 12,
  height = 8,
  units = "cm")
