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

#----------------------------------------------------------#
# 1. Load libraries and functions -----
#----------------------------------------------------------#

# delete existing workspace to start clean
rm(list = ls())

# Package version control
library(renv)
# renv::init()
# renv::snapshot(lockfile = "DATA/lock/revn.lock")
renv::restore(lockfile = "DATA/lock/revn.lock")

# libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(mgcv)
library(gratia)

# source scripts and functions
files.sources = list.files("R/functions/")
sapply(paste0("R/functions/", files.sources, sep = ""), source)


#----------------------------------------------------------#
# 2. Import data and define variables -----
#----------------------------------------------------------#

Dataset_work <-  read_rds("DATA/input/Dataset_20201026.RDS")

# variabe definition
age_treshold <-  20e3
ROC_treshold <-  2
text_size <-  7
TIME_BIN <-  500

# Colour definition
getPalette <-  colorRampPalette(brewer.pal(6, "Set2"))
pallete_1 <-  getPalette(6)
names(pallete_1) <-
  c("North America",
    "Latin America",
    "Europe",
    "Africa",
    "Asia",
    "Oceania")

# region boundaries definition
region_coord <- 
  tibble(
    REGION = names(pallete_1),
    long_min = c(-165, -100, -8, -18, 40, 95),
    long_max = c(-55, -37, 40, 50, 182, 175),
    lat_min = c(25, -54, 38, -32, 11, -45),
    lat_max = c(68, 25, 70, 35, 75, 10)
  )

#----------------------------------------------------------#
# 3. Prepare dataset -----
#----------------------------------------------------------#

Data_RoC <- 
  Dataset_work %>%
  unnest(cols = c(ROC_MAIN)) %>%
  mutate(BIN = ceiling(AGE / TIME_BIN) * TIME_BIN) %>%
  dplyr::select(REGION, dataset.id, long, lat, elev, BIN, AGE, ROC, PEAK)

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
      dplyr::select(REGION, dataset.id, BIN, PEAK) %>%
      ungroup() %>%
      group_by(REGION, dataset.id, BIN) %>%
      dplyr::summarise(.groups = "keep",
                       PEAK_m = max(PEAK)) %>%
      group_by(REGION, BIN) %>%
      dplyr::summarise(
        .groups = "keep",
        N = n(),
        P_value_mean = mean(PEAK_m),
        P_value_sd = sd(PEAK_m),
        P_value_se = P_value_sd / sqrt(N) ),
    by = c("REGION", "BIN") ) %>%
  mutate(
    P_value_mean = replace(P_value_mean, is.na(P_value_mean), 0),
    P_value_sd = replace(P_value_sd, is.na(P_value_sd), 0) )


#----------------------------------------------------------#
# 3. Figure 2 -----
#----------------------------------------------------------#

P_Afrika <- 
  draw.region(
    region = "Africa",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_Asia <- 
  draw.region(
    region = "Asia",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_EU <-
  draw.region(
    region = "Europe",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_LA <- 
  draw.region(
    region = "Latin America",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_NA <- 
  draw.region(
    region = "North America",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)

P_Oceania <- 
  draw.region(
    region = "Oceania",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum)


FIGURE_02 <- 
  ggarrange(
    P_NA$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) + 
      rremove("xylab") +
      rremove("x.text") + 
      rremove("x.ticks"),
    P_EU$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") +
      rremove("x.text") +
      rremove("x.ticks"),
    P_Asia$plot +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") +
      rremove("x.text") +
      rremove("x.ticks"),
    P_LA$plot +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    P_Afrika$plot +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    P_Oceania$plot +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab"),
    ncol = 3,
    nrow = 2,
    labels = c("A", "B", "C", "D", "E", "F"),
    font.label = list(size = text_size) ) %>%
  annotate_figure(
    bottom = text_grob("Age (ka yr BP) ", size = text_size),
    left = text_grob("ROC Score, 95% quantile", size = text_size, rot = 90),
    right = text_grob(" Proportion of peak points", size = text_size, rot = -90) )

ggsave(
  "figures/FIGURE_02.pdf",
  FIGURE_02,
  width = 12,
  height = 8,
  units = "cm")

#----------------------------------------------------------#
# 4. Figure S01 : Sample density ----- 
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

P_EU_samples <-
  draw.region(
    region = "Europe",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum,
    blue_samples = T)

P_LA_samples <- 
  draw.region(
    region = "Latin America",
    plot_data = Data_RoC ,
    BIN_data = Data_RoC_sum,
    blue_samples = T)

P_NA_samples <- 
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
  P_NA_samples$plot + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    rremove("xylab") +
    rremove("x.text") +
    rremove("x.ticks"),
  P_EU_samples$plot +
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    rremove("xylab") + 
    rremove("x.text") + 
    rremove("x.ticks"),
  P_Asia_samples$plot + 
    theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    rremove("xylab") + 
    rremove("x.text") + 
    rremove("x.ticks"),
  P_LA_samples$plot + 
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
    bottom = text_grob("Age (ka yr BP) ", size = text_size),
    left = text_grob("ROC Score, 95% quantile", size = text_size, rot = 90),
    right = text_grob(" Proportion of peak points", size = text_size, rot = -90) )

ggsave(
  "figures/FIGURE_S01.pdf",
  FIGURE_S01,
  width = 12,
  height = 12,
  units = "cm"
)


#----------------------------------------------------------#
# 5. Figure S02 : Sensitivity analyses -----
#----------------------------------------------------------#

TIME_BIN_sensitivity <-  250

Data_RoC_sensitivity  <-
  Dataset_work %>%
  unnest(cols = c(ROC_sens)) %>%
  mutate(BIN = ceiling(AGE / TIME_BIN_sensitivity) * TIME_BIN_sensitivity) %>%
  dplyr::select(REGION,
                dataset.id,
                long, lat,
                elev,
                BIN,
                AGE,
                ROC,
                PEAK)

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
      dplyr::select(REGION, dataset.id, BIN, PEAK) %>%
      ungroup() %>%
      group_by(REGION, dataset.id, BIN) %>%
      dplyr::summarise(.groups = "keep",
                       PEAK_m = max(PEAK)) %>%
      group_by(REGION, BIN) %>%
      dplyr::summarise(
        .groups = "keep",
        N = n(),
        P_value_mean = mean(PEAK_m),
        P_value_sd = sd(PEAK_m),
        P_value_se = P_value_sd / sqrt(N) ),
    by = c("REGION", "BIN") ) %>%
  mutate(
    P_value_mean = replace(P_value_mean, is.na(P_value_mean), 0),
    P_value_sd = replace(P_value_sd, is.na(P_value_sd), 0) )


P_Afrika_sensitivity <-
  draw.region(
    region = "Africa",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_Asia_sensitivity <- 
  draw.region(
    region = "Asia",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_EU_sensitivity <- 
  draw.region(
    region = "Europe",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_LA_sensitivity <- 
  draw.region(
    region = "Latin America",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_NA_sensitivity <- 
  draw.region(
    region = "North America",
    plot_data = Data_RoC_sensitivity ,
    BIN_data = Data_RoC_sum_sensitivity)

P_Oceania_sensitivity <- 
  draw.region(region = "Oceania",
              plot_data = Data_RoC_sensitivity ,
              BIN_data = Data_RoC_sum_sensitivity)


FIGURE_S02 <- 
  ggarrange(
    P_NA_sensitivity$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_EU_sensitivity$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + rremove("x.ticks"),
    P_Asia_sensitivity$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_LA_sensitivity$plot + 
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
    bottom = text_grob("Age (ka yr BP) ", size = text_size),
    left = text_grob("ROC Score, 95% quantile", size = text_size, rot = 90),
    right = text_grob(" Proportion of peak points", size = text_size, rot = -90) )

ggsave(
  "figures/FIGURE_S02.pdf",
  FIGURE_S02,
  width = 12,
  height = 8,
  units = "cm"
)


#----------------------------------------------------------#
# 6. Figure S03 : Exclude rare taxa -----
#----------------------------------------------------------#

Data_RoC_common_taxa  <- 
  Dataset_work %>%
  unnest(cols = c(ROC_richness)) %>%
  mutate(BIN = ceiling(AGE / TIME_BIN) * TIME_BIN) %>%
  dplyr::select(REGION, 
                dataset.id, 
                long, 
                lat, 
                elev, 
                BIN, 
                AGE, 
                ROC, 
                PEAK)

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
      dplyr::select(REGION, dataset.id, BIN, PEAK) %>%
      ungroup() %>%
      group_by(REGION, dataset.id, BIN) %>%
      dplyr::summarise(.groups = "keep",
                       PEAK_m = max(PEAK)) %>%
      group_by(REGION, BIN) %>%
      dplyr::summarise(
        .groups = "keep",
        N = n(),
        P_value_mean = mean(PEAK_m),
        P_value_sd = sd(PEAK_m),
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

P_EU_common_taxa <- 
  draw.region(
    region = "Europe",
    plot_data = Data_RoC_common_taxa ,
    BIN_data = Data_RoC_sum_common_taxa,
    RoC_max_value = 1.2,
    axis_ratio = 2 )

P_LA_common_taxa <-
  draw.region(
    region = "Latin America",
    plot_data = Data_RoC_common_taxa ,
    BIN_data = Data_RoC_sum_common_taxa,
    RoC_max_value = 1.2,
    axis_ratio = 2)

P_NA_common_taxa <- 
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
    P_NA_common_taxa$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_EU_common_taxa$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_Asia_common_taxa$plot + 
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      rremove("xylab") + 
      rremove("x.text") + 
      rremove("x.ticks"),
    P_LA_common_taxa$plot + 
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
    bottom = text_grob("Age (ka yr BP) ", size = text_size),
    left = text_grob("ROC Score, 95% quantile", size = text_size, rot = 90),
    right = text_grob(" Proportion of peak points", size = text_size, rot = -90) )

ggsave(
  "figures/FIGURE_S03.pdf",
  FIGURE_S03,
  width = 12,
  height = 8,
  units = "cm")

#----------------------------------------------------------#
# 7. Estimate the first increase of RoC in Late Holocene -----
#----------------------------------------------------------#

region_ROC_increase <- 
  tibble(REGION = names(pallete_1), 
         start_of_increae = c(extract.first.increase(P_NA$data_ROC),
                              extract.first.increase(P_LA$data_ROC),
                              extract.first.increase(P_EU$data_ROC),
                              try(extract.first.increase(P_Afrika$data_ROC), silent = T),
                              try(extract.first.increase(P_Asia$data_ROC), silent = T),
                              extract.first.increase(P_Oceania$data_ROC)),
         Mod_explained = c(try(extract.explained.variability(P_NA$model_ROC), silent = T),
                           try(extract.explained.variability(P_LA$model_ROC), silent = T),
                           try(extract.explained.variability(P_EU$model_ROC), silent = T),
                           try(extract.explained.variability(P_Afrika$model_ROC), silent = T),
                           try(extract.explained.variability(P_Asia$model_ROC), silent = T),
                           try(extract.explained.variability(P_Oceania$model_ROC), silent = T)),
         start_of_increae_sensitivity = c(extract.first.increase(P_NA_sensitivity$data_ROC),
                                          extract.first.increase(P_LA_sensitivity$data_ROC),
                                          extract.first.increase(P_EU_sensitivity$data_ROC),
                                          extract.first.increase(P_Afrika_sensitivity$data_ROC),
                                          extract.first.increase(P_Asia_sensitivity$data_ROC),
                                          extract.first.increase(P_Oceania_sensitivity$data_ROC)),
         Mod_explained_sens = c(extract.explained.variability(P_NA_sensitivity$model_ROC),
                                extract.explained.variability(P_LA_sensitivity$model_ROC),
                                extract.explained.variability(P_EU_sensitivity$model_ROC),
                                extract.explained.variability(P_Afrika_sensitivity$model_ROC),
                                extract.explained.variability(P_Asia_sensitivity$model_ROC),
                                extract.explained.variability(P_Oceania_sensitivity$model_ROC))
  )

write.csv(region_ROC_increase,"DATA/output/region_ROC_increase.csv")
