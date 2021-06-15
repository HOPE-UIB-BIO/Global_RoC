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

# load information of the clusters. For their calculation, see branch V.1.0
cluster_info <- read_rds("DATA/input/cluster_info.rds")

#----------------------------------------------------------#
# 1. Estimate patterns per continent -----
#----------------------------------------------------------#

# EUROPE
cluster_Europe <- 
  Dataset_work %>% 
  dplyr::filter(
    REGION  == "Europe") %>% 
  left_join(
    cluster_info,
    by = "dataset.id")

cluster_curves_Europe_merged <- 
  calcuate.RoC.for.cluster(cluster_Europe)


# North America
cluster_North_America_merged <- 
  Dataset_work %>% 
  dplyr::filter(
    REGION  == "North America") %>% 
  left_join(
    cluster_info,
    by = "dataset.id")


cluster_curves_North_America_merged <-
  calcuate.RoC.for.cluster(cluster_North_America_merged)

# Latin America
cluster_Latin_America <- 
  Dataset_work %>% 
  dplyr::filter(
    REGION  == "Latin America") %>% 
  left_join(
    cluster_info,
    by = "dataset.id")

cluster_curves_Latin_America_merged <-
  calcuate.RoC.for.cluster(cluster_Latin_America)

# Asia
cluster_Asia <-
  Dataset_work %>% 
  dplyr::filter(
    REGION  == "Asia") %>% 
  left_join(
    cluster_info,
    by = "dataset.id")

cluster_curves_Asia_merged <-
  calcuate.RoC.for.cluster(cluster_Asia)


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
                        region = as.character(u),
                        siluete = F,
                        palette_x = Palette_Euroasia,
                        deriv = T,# 1.75
                        y_cut = 0.95, # 1.3  / 0.9
                        y_start = 0.2, # 0 / 0.32 
                        axis_ratio = 1.1 # 2 / 1.2 
                        )
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
                                 y_cut = 0.95, # 1.3  / 0.9
                                 y_start = 0.2, # 0 / 0.32 
                                 axis_ratio = 1.1 # 2 / 1.2 
                                 )
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

