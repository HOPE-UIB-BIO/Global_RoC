#----------------------------------------------------------#
#
#
#                 Global Rate-of-Change patterns
#
#                   RoC continetal patterns
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
# 2.1 Sensitivity analyses -----
#----------------------------------------------------------#

Data_RoC_sensitivity  <-
  Dataset_work %>%
  unnest(cols = c(ROC_sens)) %>%
  mutate(BIN = ceiling(Age / time_bin_sensitivity) * time_bin_sensitivity) %>%
  dplyr::select(REGION,
                dataset.id,
                long, lat,
                elev,
                BIN,
                Age,
                ROC,
                Peak)

Data_RoC_sum_sensitivity <- 
  Data_RoC_sensitivity %>%
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
    Data_RoC_sensitivity %>%
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


P_Africa_sensitivity <-
  draw.region(
    region = "Africa",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_Asia_sensitivity <- 
  draw.region(
    region = "Asia",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_Europe_sensitivity <- 
  draw.region(
    region = "Europe",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_Latin_America_sensitivity <- 
  draw.region(
    region = "Latin America",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_North_America_sensitivity <- 
  draw.region(
    region = "North America",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_Oceania_sensitivity <- 
  draw.region(region = "Oceania",
              plot_data = Data_RoC_sensitivity ,
              BIN_data = Data_RoC_sum_sensitivity)

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
      "_"),
    sensitivity_plot_name = paste0(plot_name,"_sensitivity")) %>% 
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
      digits = 2),
    
    # Start_of_increase_sensitivity = purrr::map_dbl(
    #   sensitivity_plot_name, 
    #   possibly(
    #     function(x){extract.first.increase(get(x))},
    #     NA_real_)),
    # 
    # Mod_explained_sensitivity = purrr::map_dbl(
    #   sensitivity_plot_name, 
    #   function(x){extract.explained.variability(get(x))})
    
  ) %>% 
  dplyr::select(-c(plot_name,sensitivity_plot_name)) 


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
          y_cut = 1.3,
          axis_ratio = 2
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


#----------------------------------------------------------#
# 3. Figure S01 : Sample density ----- 
#----------------------------------------------------------#

P_Afrika_samples <- 
  draw.region(
    region = "Africa",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum,
    blue_samples = T )

P_Asia_samples <- 
  draw.region(
    region = "Asia",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum,
    blue_samples = T)

P_Europe_samples <-
  draw.region(
    region = "Europe",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum,
    blue_samples = T)

P_Latin_America_samples <- 
  draw.region(
    region = "Latin America",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum,
    blue_samples = T)

P_North_America_samples <- 
  draw.region(
    region = "North America",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum,
    blue_samples = T)

P_Oceania_samples <- 
  draw.region(
    region = "Oceania",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum,
    blue_samples = T)


FIGURE_S01 <- ggarrange(
  P_North_America_samples$plot + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    rremove("xylab") +
    rremove("x.text") +
    rremove("x.ticks"),
  P_Europe_samples$plot +
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    rremove("xylab") + 
    rremove("x.text") + 
    rremove("x.ticks"),
  P_Asia_samples$plot + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    rremove("xylab") + 
    rremove("x.text") + 
    rremove("x.ticks"),
  P_Latin_America_samples$plot + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    rremove("xylab"),
  P_Afrika_samples$plot + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    rremove("xylab"),
  P_Oceania_samples$plot + 
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
  "figures/FIGURE_S01.pdf",
  FIGURE_S01,
  width = 12,
  height = 12,
  units = "cm"
)

#----------------------------------------------------------#
# 4. Figure S02 : sensitivity ----- 
#----------------------------------------------------------#

FIGURE_S02 <- 
  ggarrange(
    P_North_America_sensitivity$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_Europe_sensitivity$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + rremove("x.ticks"),
    P_Asia_sensitivity$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_Latin_America_sensitivity$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    P_Afrika_sensitivity$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    P_Oceania_sensitivity$plot + 
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
  "figures/FIGURE_S02.pdf",
  FIGURE_S02,
  width = 12,
  height = 8,
  units = "cm"
)


#----------------------------------------------------------#
# 5. Figure S03 : Exclude rare taxa -----
#----------------------------------------------------------#

Data_RoC_common_taxa  <- 
  Dataset_work %>%
  unnest(cols = c(ROC_richness)) %>%
  mutate(BIN = ceiling(Age / time_bin) * time_bin) %>%
  dplyr::select(REGION, 
                dataset.id, 
                long, 
                lat, 
                elev, 
                BIN, 
                Age, 
                ROC, 
                Peak)

Data_RoC_sum_common_taxa <- 
  Data_RoC_common_taxa %>%
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
    Data_RoC_common_taxa %>%
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


P_Afrika_common_taxa <-
  draw.region(
    region = "Africa",
    plot_data = Data_RoC_common_taxa ,
    BIN_data = Data_RoC_sum_common_taxa,
    RoC_max_value = 1.2,
    axis_ratio = 2)

P_Asia_common_taxa <- 
  draw.region(
    region = "Asia",
    plot_data = Data_RoC_common_taxa ,
    BIN_data = Data_RoC_sum_common_taxa,
    RoC_max_value = 1.2,
    axis_ratio = 2)

P_Europe_common_taxa <- 
  draw.region(
    region = "Europe",
    plot_data = Data_RoC_common_taxa ,
    BIN_data = Data_RoC_sum_common_taxa,
    RoC_max_value = 1.2,
    axis_ratio = 2 )

P_Latin_America_common_taxa <-
  draw.region(
    region = "Latin America",
    plot_data = Data_RoC_common_taxa ,
    BIN_data = Data_RoC_sum_common_taxa,
    RoC_max_value = 1.2,
    axis_ratio = 2)

P_North_America_common_taxa <- 
  draw.region(
    region = "North America",
    plot_data = Data_RoC_common_taxa ,
    BIN_data = Data_RoC_sum_common_taxa,
    RoC_max_value = 1.2,
    axis_ratio = 2)

P_Oceania_common_taxa <- 
  draw.region(
    region = "Oceania",
    plot_data = Data_RoC_common_taxa ,
    BIN_data = Data_RoC_sum_common_taxa,
    RoC_max_value = 1.2,
    axis_ratio = 2)


FIGURE_S03 <- 
  ggarrange(
    P_North_America_common_taxa$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_Europe_common_taxa$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_Asia_common_taxa$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_Latin_America_common_taxa$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    P_Afrika_common_taxa$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    P_Oceania_common_taxa$plot + 
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
  "figures/FIGURE_S03.pdf",
  FIGURE_S03,
  width = 12,
  height = 8,
  units = "cm")



#----------------------------------------------------------#
# 6. Figure S06: Sequence distribution -----
#----------------------------------------------------------#

FIGURE_S06_plot_list <-
  tibble(REGION = names(pallete_1)[c(1,3,5,2,4,6)]) %>% 
  mutate(plot = purrr::map(
    REGION,
    .f = function(x){
      
      data_lines <- 
        Data_RoC %>% 
        filter(REGION == x)
      
      n_sites <-
        data_lines$dataset.id %>% 
        unique() %>% 
        length()
      
      data_points <-
        Data_RoC_sum %>% 
        filter(REGION == x)
      
      p_lines <-
        ggplot()+
        scale_x_continuous(
          trans = "reverse",
          breaks = seq(0, 20e3, 2e3),
          labels = seq(0, 20, 2)) +
        scale_y_continuous(
          limits = c(-0.1, 1.3),
          breaks = seq(0, 1.3, 0.2))+
        coord_cartesian(xlim = c(age_treshold, 0)) +
        scale_color_manual(values =pallete_1)+
        theme_classic() +
        theme(
          legend.position = "none",
          text = element_text(size = text_size),
          panel.border = element_rect(
            fill = NA,
            colour = "gray30",
            size = 0.1)) +
        labs(x = "",
             y= "")+
        geom_vline(
          xintercept = seq(0,age_treshold,500),
          color = "gray90",
          size = 0.1)+
        geom_line(
          data = data_lines,
          aes(
            x=Age, 
            y= ROC,
            group=dataset.id),
          alpha=1/10,
          size = 0.1)+
        geom_point(
          data = data_points,
          aes(
            x = BIN,
            y= ROC_upq,
            colour = REGION),
          shape = 15,
          size = 1)+
        geom_point(
          data = data_points,
          aes(
            x = BIN,
            y= ROC_median,
            colour = REGION),
          shape = 17,
          size = 1)
      
      return(p_lines)
      
    }))

FIGURE_S06 <-
  ggarrange(plotlist = FIGURE_S06_plot_list$plot,
            labels = LETTERS[1:6],
            font.label = list(size = text_size),
            nrow = 2, 
            ncol= 3) %>% 
  annotate_figure(bottom = text_grob("Age (ka) ", size = text_size),
                  left = text_grob("ROC Score", size = text_size, rot = 90))

ggsave(
  "figures/FIGURE_S06.pdf",
  FIGURE_S06,
  width = 12,
  height = 8,
  units = "cm")



