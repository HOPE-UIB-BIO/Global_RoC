#----------------------------------------------------------#
#
#
#                 Global Rate-of-Change patterns
#                       
#                   RoC sub-regional patterns 
#
#                     Ondrej Mottl - 2020
#
#----------------------------------------------------------#

source("R/00_config.R")

#----------------------------------------------------------#
# 1. Estimate patterns per continent -----
#----------------------------------------------------------#

# EUROPE
cluster_Europe <- 
  draw.cluster(
    region = "Europe",
    cluster_method = cluster_method, 
    distance = distance)

plot(cluster_Europe$plot)

cluster_curves_Europe_merged <- 
  calcuate.RoC.for.cluster(cluster_Europe$data_sites)


# North America
cluster_North_America <- 
  draw.cluster(
    region = "North America",
    cluster_method = cluster_method, 
    distance = distance)

plot(cluster_North_America$plot)

cluster_curves_North_America <-
  calcuate.RoC.for.cluster(cluster_North_America$data_sites)

cluster_North_America_merged <-
  cluster_North_America$data_sites %>%
  mutate(cluster = as.character(cluster)) %>% 
  mutate(
    cluster = ifelse(cluster == "10", "3",cluster),
    cluster = ifelse(cluster == "7", "3",cluster),
    cluster = ifelse(cluster == "9", "3",cluster) ) %>% 
  mutate(cluster = as.factor(cluster))

cluster_curves_North_America_merged <-
  calcuate.RoC.for.cluster(cluster_North_America_merged)

# Latin America
cluster_Latin_America <- 
  draw.cluster(
    region = "Latin America",
    cluster_method = cluster_method, 
    distance = distance)

plot(cluster_Latin_America$plot)

cluster_curves_Latin_America_merged <-
  calcuate.RoC.for.cluster(cluster_Latin_America$data_sites)

# Asia
cluster_Asia <-
  draw.cluster(
    region = "Asia",
    cluster_method = cluster_method, 
    distance = distance)

plot(cluster_Asia$plot)

cluster_curves_Asia_merged <-
  calcuate.RoC.for.cluster(cluster_Asia$data_sites)


#----------------------------------------------------------#
# 2. Figure 3: Euroasia -----
#----------------------------------------------------------#

EUROASIA_map_data <-  
  bind_rows(
    cluster_curves_Asia_merged$data_sites,
    cluster_curves_Europe_merged$data_sites) %>%
  mutate(C_comb = paste0(REGION,"-",cluster ) )

EUROASIA_map <-  
  EUROASIA_map_data %>%
  ggplot(aes(x=long, y=lat))+
  borders(fill = "gray90", colour = "gray90") +
  coord_quickmap(xlim=c(min(EUROASIA_map_data$long),
                        max(EUROASIA_map_data$long)),
                 ylim = c(min(EUROASIA_map_data$lat),
                          max(EUROASIA_map_data$lat)))+
  geom_point(aes(color=C_comb), alpha=1, size=0.5, shape=20)+
  theme_classic()+
  scale_color_manual(values = Palette_Euroasia)+
  labs(x= "longitude",
       y= "latitude")+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.line = element_blank())

ggsave(
  "figures/FIGURE_03_A.pdf",
  EUROASIA_map,
  width = 12,
  height = 9,
  units = "cm")

EUROASIA_curves  <-  
  bind_rows(
    tibble(
      REGION = "Asia",
      cluster_curves_Asia_merged$data_c),
    tibble(
      REGION = "Europe",
      cluster_curves_Europe_merged$data_c)) %>%
  mutate(C_comb = paste0(REGION,"-",C_char ) ) %>%
  mutate(Plot = purrr::pmap(
    list(data,
         pred_gam_ROC_upq,
         pred_gam_ROC_Peak,
         C_comb),
    .f=function(x,y,z,u){ 
      plot_res  <-   
        draw.gam.custom(x,y,z,
                        region =as.character(u),
                        siluete = F,
                        palette_x = Palette_Euroasia,
                        deriv = T,
                        y_cut=1.75)
      return(plot_res)
    }))

EUROASIA_curve_plot  <-
  ggarrange(
    plotlist = EUROASIA_curves$Plot,
    ncol= 3,
    nrow = 4,
    labels = LETTERS[1:nrow(EUROASIA_curves)],
    font.label = list(size=text_size))

ggsave(
  "figures/FIGURE_03_B.pdf",
  EUROASIA_curve_plot,
  width = 9,
  height = 9,
  units = "cm")

#----------------------------------------------------------#
# 3. Figure S4: Euroasia enviromental prop. -----
#----------------------------------------------------------#

FIGURE_S04  <- 
  ggarrange(
    EUROASIA_map,
    EUROASIA_map_data %>%
      dplyr::select(lat,
                    long,
                    elev,
                    MAT,
                    T.var,
                    Perc.dry,
                    Perc.var,
                    C_comb) %>%
      mutate(C_comb = as.factor(C_comb)) %>%
      mutate(C_comb = fct_reorder(C_comb, long)) %>%
      rename(Latitude = lat,
             Longitude = long,
             Elevation = elev,
             Mean_annual_temperature = MAT,
             Temperature_seasonality = T.var,
             Precipitation_of_the_driest_quatrer = Perc.dry,
             Precipitation_seasonality = Perc.var) %>%
      pivot_longer(cols = -c(C_comb)) %>%
      ggplot(aes(
        x=C_comb,
        y= value,
        color=C_comb,
        fill= C_comb))+
      geom_boxplot(
        width=0.5,
        size=0.1, 
        outlier.size = 0.1)+
      facet_wrap(~name,scales = "free_y" )+
      scale_color_manual(values = Palette_Euroasia)+
      scale_fill_manual(values = Palette_Euroasia)+
      theme_classic()+
      theme(line = element_line(size= 0.1),
            legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            text = element_text(size = text_size),
            panel.grid = element_blank(),
            panel.border = element_rect(
              fill=NA,
              colour = "gray30",
              size=0.1))+
      labs(x=""),
    nrow = 2)


ggsave(
  "figures/FIGURE_S04.pdf",
  FIGURE_S04,
  width = 17,
  height = 14, 
  units = "cm")

#----------------------------------------------------------#
# 4. Figure 4 -----
#----------------------------------------------------------#

AMERICAS_map_data <-  
  bind_rows(
    cluster_curves_North_America_merged$data_sites,
    cluster_curves_Latin_America_merged$data_sites) %>%
  mutate(C_comb = paste0(REGION,"-",cluster ) )


AMERICAS_map <- 
  AMERICAS_map_data %>%
  ggplot(aes(x=long, y=lat))+
  borders(fill = "gray90", colour = "gray90") +
  coord_quickmap(xlim=c(min(AMERICAS_map_data$long),
                        max(AMERICAS_map_data$long)),
                 ylim = c(min(AMERICAS_map_data$lat),
                          max(AMERICAS_map_data$lat)))+
  geom_point(aes(
    color=C_comb),
    alpha=1,
    size=0.5,
    shape=20)+
  theme_classic()+
  scale_color_manual(values = Palette_Americas)+
  labs(y= "latitude",
       x= "longitude")+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.line = element_blank())

ggsave(
  "figures/FIGURE_04_A.pdf",
  AMERICAS_map,
  width = 12,
  height = 9, 
  units = "cm")

AMERICAS_curves <-  
  bind_rows(
    tibble(
      REGION = "North America",
      cluster_curves_North_America_merged$data_c),
    tibble(
      REGION = "Latin America",
      cluster_curves_Latin_America_merged$data_c)) %>%
  mutate(C_comb = paste0(REGION,"-",C_char ) ) %>%
  mutate(Plot = purrr::pmap(list(
    data, 
    pred_gam_ROC_upq,
    pred_gam_ROC_Peak,
    C_comb),
    .f=function(x,y,z,u){
      plot_res = draw.gam.custom(x,y,z,
                                 region =as.character(u),
                                 siluete = F,
                                 palette_x = Palette_Americas,
                                 deriv = T,
                                 y_cut=1.75)
      return(plot_res)
    }))

AMERICAS_curve_plot <-  
  ggarrange(
    plotlist = AMERICAS_curves$Plot,
    ncol= 3,
    nrow = 4,
    labels = LETTERS[1:nrow(AMERICAS_curves)],
    font.label = list(size=text_size))

ggsave(
  "figures/FIGURE_04_B.pdf",
  AMERICAS_curve_plot,
  width = 9,
  height = 9, 
  units = "cm")

#----------------------------------------------------------#
# 5. Figure S5 -----
#----------------------------------------------------------#

FIGURE_S05 <- 
  ggarrange(
    AMERICAS_map,
    AMERICAS_map_data %>%
      dplyr::select(lat,
                    long,
                    elev,
                    MAT,
                    T.var,
                    Perc.dry,
                    Perc.var,
                    C_comb) %>%
      mutate(C_comb = as.factor(C_comb)) %>%
      mutate(C_comb = fct_reorder(C_comb, long )) %>%
      rename(
        Latitude = lat,
        Longitude = long,
        Elevation = elev,
        Mean_annual_temperature = MAT,
        Temperature_seasonality = T.var,
        Precipitation_of_the_driest_quatrer = Perc.dry,
        Precipitation_seasonality = Perc.var
      ) %>%
      pivot_longer(cols = -c(C_comb)) %>%
      ggplot(aes(
        x=C_comb,
        y= value,
        color=C_comb,
        fill= C_comb))+
      geom_boxplot( width=0.5, size=0.1, outlier.size = 0.1 )+
      facet_wrap(~name,scales = "free_y" )+
      scale_color_manual(values = Palette_Americas)+
      scale_fill_manual(values = Palette_Americas)+
      theme_classic()+
      theme(line = element_line( size = 0.1 ),
            legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            text = element_text(size = text_size),
            panel.grid = element_blank(),
            panel.border = element_rect(fill=NA, colour = "gray30", size=0.1))+
      labs(x=""),
    nrow = 2)

ggsave(
  "figures/FIGURE_S05.pdf",
  FIGURE_S05,
  width = 17,
  height = 14,
  units = "cm")


#----------------------------------------------------------#
# 6. Non-merged figure -----
#----------------------------------------------------------#

AMERICAS_map_data_nonmerged <-  
  bind_rows(
    cluster_curves_North_America$data_sites,
    cluster_curves_Latin_America_merged$data_sites) %>%
  mutate(C_comb = paste0(REGION,"-",cluster ) )

Palette_Americas_nonmerged <- 
  AMERICAS_map_data_nonmerged$C_comb %>% 
  unique() %>% 
  length() %>% 
  getPalette()

names(Palette_Americas_nonmerged) <-
  AMERICAS_map_data_nonmerged$C_comb %>% 
  unique()

AMERICAS_map_nonmerged <- 
  AMERICAS_map_data_nonmerged %>%
  ggplot(aes(x=long, y=lat))+
  borders(fill = "gray90", colour = "gray90") +
  coord_quickmap(xlim=c(min(AMERICAS_map_data$long),
                        max(AMERICAS_map_data$long)),
                 ylim = c(min(AMERICAS_map_data$lat),
                          max(AMERICAS_map_data$lat)))+
  geom_point(aes(
    color=C_comb),
    alpha=1,
    size=0.5,
    shape=20)+
  theme_classic()+
  scale_color_manual(values = Palette_Americas_nonmerged)+
  labs(y= "latitude",
       x= "longitude")+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.line = element_blank())

AMERICAS_curves_nonmerged <-  
  bind_rows(
    tibble(
      REGION = "North America",
      cluster_curves_North_America$data_c),
    tibble(
      REGION = "Latin America",
      cluster_curves_Latin_America_merged$data_c)) %>%
  mutate(C_comb = paste0(REGION,"-",C_char ) ) %>%
  mutate(Plot = purrr::pmap(list(
    data, 
    pred_gam_ROC_upq,
    pred_gam_ROC_Peak,
    C_comb),
    .f=function(x,y,z,u){
      plot_res = draw.gam.custom(x,y,z,
                                 region =as.character(u),
                                 siluete = F,
                                 palette_x = Palette_Americas_nonmerged,
                                 deriv = T,
                                 y_cut=1.75)
      return(plot_res)
    }))

AMERICAS_curve_plot_nonmerged <-  
  ggarrange(
    plotlist = AMERICAS_curves_nonmerged$Plot,
    ncol= 3,
    nrow = 4,
    labels = LETTERS[1:nrow(AMERICAS_curves_nonmerged)],
    font.label = list(size=text_size))

FIGURE_S07 <- 
  ggarrange(
    AMERICAS_map_nonmerged,
    AMERICAS_curve_plot_nonmerged,
    nrow = 1)

ggsave(
  "figures/FIGURE_S07.pdf",
  FIGURE_S07,
  width = 20,
  height = 9,
  units = "cm")



#----------------------------------------------------------#
# 7. Sensitivity analyses -----
#----------------------------------------------------------#

# # EUROPE
# cluster_curves_Europe_sens <- 
#   calcuate.RoC.for.cluster(
#     cluster_Europe$data_sites,
#     bin_size = time_bin_sensitivity,
#     ROC_metric = "ROC_sens")
# 
# # North America
# cluster_curves_North_America_sens <- 
#   calcuate.RoC.for.cluster(
#     cluster_North_America_merged,
#     bin_size = time_bin_sensitivity,
#     ROC_metric = "ROC_sens")
# 
# # Latin America
# cluster_curves_Latin_America_sens <- 
#   calcuate.RoC.for.cluster(
#     cluster_Latin_America$data_sites,
#     bin_size = time_bin_sensitivity,
#     ROC_metric = "ROC_sens")
# 
# # Asia
# cluster_curves_Asia_sens <- 
#   calcuate.RoC.for.cluster(
#     cluster_Asia$data_sites,
#     bin_size = time_bin_sensitivity,
#     ROC_metric = "ROC_sens")

#----------------------------------------------------------#
# 8. Model details tables -----
#----------------------------------------------------------#

Euroasia_translation_tibble <-
  tibble(
    original = LETTERS[1:nrow(EUROASIA_curves)],
    final = c("D","I","J","B","A","C","E","H","F","G"))


Euroasia_model_tibble <-
  bind_rows(
    tibble(
      REGION = "Asia",
      cluster_curves_Asia_merged$data_c),
    tibble(
      REGION = "Europe",
      cluster_curves_Europe_merged$data_c)) %>%
  mutate(start_of_increase = purrr::map_dbl(
    pred_gam_ROC_upq,
    possibly(function(x) extract.first.increase(x, nested = F),
             otherwise = NA))) %>% 
  mutate(Mod_explained = purrr::map_dbl(
    gam_ROC_upq,
    possibly(function(x) extract.explained.variability(x, nested = F),
             otherwise = NA))) %>% 
  mutate(original = LETTERS[1:nrow(EUROASIA_curves)]) %>% 
  left_join(Euroasia_translation_tibble, by= "original") %>% 
  rename(Cluster = final) %>% 
  arrange(Cluster) %>% 
  dplyr::select(REGION, Cluster, start_of_increase, Mod_explained)  

# Euroasia_model_tibble_sens <-
#   bind_rows(tibble(
#     REGION = "Asia",
#     cluster_curves_Asia_sens$data_c),
#     tibble(
#       REGION = "Europe",
#       cluster_curves_Europe_sens$data_c)) %>%
#   mutate(start_of_increase = purrr::map_dbl(
#     pred_gam_ROC_upq,
#     possibly(function(x) extract.first.increase(x, nested = F),
#              otherwise = NA))) %>% 
#   mutate(Mod_explained = purrr::map_dbl(
#     gam_ROC_upq,
#     possibly(function(x) extract.explained.variability(x, nested = F),
#              otherwise = NA))) %>% 
#   mutate(original = LETTERS[1:nrow(EUROASIA_curves)]) %>% 
#   left_join(Euroasia_translation_tibble, by= "original") %>% 
#   rename(Cluster = final) %>% 
#   arrange(Cluster) %>% 
#   dplyr::select(REGION, Cluster, start_of_increase, Mod_explained)   %>% 
#   rename_if(is.double,~paste0(.,"_sens"))


Americas_translation_tibble <-
  tibble(
    original = LETTERS[1:nrow(AMERICAS_curves)],
    final = c("H","I","A","B","C","G","F","J","D","E"))

Americas_model_tibble <-
  bind_rows(
    tibble(
      REGION = "North America",
      cluster_curves_North_America_merged$data_c),
    tibble(
      REGION = "Latin America",
      cluster_curves_Latin_America_merged$data_c)) %>%
  mutate(start_of_increase = purrr::map_dbl(
    pred_gam_ROC_upq,
    possibly(function(x) extract.first.increase(x, nested = F),
             otherwise = NA))) %>% 
  mutate(Mod_explained = purrr::map_dbl(
    gam_ROC_upq,
    possibly(function(x) extract.explained.variability(x, nested = F),
             otherwise = NA))) %>% 
  mutate(original = LETTERS[1:nrow(AMERICAS_curves)]) %>% 
  left_join(Americas_translation_tibble , by= "original") %>% 
  rename(Cluster = final) %>% 
  arrange(Cluster) %>% 
  dplyr::select(REGION, Cluster, start_of_increase, Mod_explained)  

# Americas_model_tibble_sens <-
#   bind_rows(tibble(
#     REGION = "North America",
#     cluster_curves_North_America_sens$data_c),
#     tibble(
#       REGION = "Latin America",
#       cluster_curves_Latin_America_sens$data_c)) %>%
#   mutate(start_of_increase = purrr::map_dbl(
#     pred_gam_ROC_upq,
#     possibly(function(x) extract.first.increase(x, nested = F),
#              otherwise = NA))) %>% 
#   mutate(Mod_explained = purrr::map_dbl(
#     gam_ROC_upq,
#     possibly(function(x) extract.explained.variability(x, nested = F),
#              otherwise = NA))) %>% 
#   mutate(original = LETTERS[1:nrow(AMERICAS_curves)]) %>% 
#   left_join(Americas_translation_tibble , by= "original") %>% 
#   rename(Cluster = final) %>% 
#   arrange(Cluster) %>% 
#   dplyr::select(REGION, Cluster, start_of_increase, Mod_explained)   %>% 
#   rename_if(is.double,~paste0(.,"_sens"))


Cluster_model_tibble <-
  bind_rows(
    Euroasia_model_tibble %>% 
      left_join(Euroasia_model_tibble_sens,
                by = c("REGION", "Cluster")),
    Americas_model_tibble %>% 
      left_join(Americas_model_tibble_sens,
                by = c("REGION", "Cluster"))) %>% 
  mutate(CLUSTER = ifelse(REGION == "Europe" | REGION == "Asia",
                          paste0("Fig. 3",Cluster),
                          paste0("Fig. 4",Cluster))) %>% 
  dplyr::select(CLUSTER, start_of_increase, Mod_explained, start_of_increase_sens, Mod_explained_sens )


Cluster_model_tibble <-
  bind_rows(
    Euroasia_model_tibble,
    Americas_model_tibble ) %>% 
  mutate(CLUSTER = ifelse(REGION == "Europe" | REGION == "Asia",
                          paste0("Fig. 3", Cluster),
                          paste0("Fig. 4", Cluster))) %>% 
  dplyr::select(CLUSTER, start_of_increase, Mod_explained )




write.csv(Cluster_model_tibble,"DATA/output/cluster_ROC_increase.csv")
