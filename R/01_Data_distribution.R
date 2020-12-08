#----------------------------------------------------------#
#
#
#                 Global Rate-of-Change patterns
#
#                   Distribution of sites
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
library(maps)
library(mgcv)

#----------------------------------------------------------#
# 2. Import data and define variables -----
#----------------------------------------------------------#

Dataset_work <-  read_rds("DATA/input/Dataset_20201203.RDS")

# variable definition
text_size <-  7
time_bin <-   500

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
    lat_max = c(68, 25, 70, 35, 75, 10) )

#-------------------------------------------#
# 2.1 Export sites dataset -----
#-------------------------------------------#

Dataset_work %>%
  dplyr::select(dataset.id,
                site.name,
                lat,
                long,
                elev,
                REGION,
                pubs,
                MAT,
                T.var,
                Perc.dry,
                Perc.var) %>%
  rename(
    Sequence_name = site.name,
    Latitude = lat,
    Longitude = long,
    Elevation = elev,
    References = pubs,
    Mean_annual_temperature = MAT,
    Temperature_seasionality = T.var,
    Precipitation_of_the_driest_quatrer = Perc.dry,
    Precipitation_seasonality = Perc.var) %>%
  write.csv("DATA/output/Dataset_table.csv")


#-------------------------------------------#
# 2.2 Data distribution -----
#-------------------------------------------#

Site_distribution <- 
  Dataset_work %>%
  group_by(REGION) %>%
  summarise(.groups = "keep",
            N = n())

# Data RoC
Data_RoC  <- 
  Dataset_work %>%
  unnest(cols = c(ROC_MAIN)) %>%
  dplyr::select(REGION, dataset.id, long, lat, elev, Age, ROC, Peak)

fig_distribution_time <- 
  Data_RoC %>%
  left_join(., Site_distribution, by = "REGION") %>%
  ggplot(aes(x = reorder(REGION,-N), y = Age)) +
  geom_hline(
    yintercept = c(4.2e3, 8.2e3, 11.7e3, 17e3),
    color = "gray60",
    size = 0.1,
    lty = 1) +
  geom_violin(aes(fill = REGION), color = NA, trim = T) +
  geom_boxplot(
    aes(group = REGION),
    color = "gray30",
    width = 0.1,
    size = 0.1,
    outlier.size = 0.1,
    outlier.shape = NA) +
  geom_text(
    data = Site_distribution,
    aes(label = N, x = REGION, y = -1700),
    color = "gray30",
    size = 2,
    angle = 90) +
  coord_cartesian(ylim = c(20e3, -3e3)) +
  scale_y_continuous(
    trans = "reverse",
    breaks = seq(0, 20e3, 4e3),
    labels = seq(0, 20, 4) ) +
  scale_fill_manual(values = pallete_1) +
  theme_bw() +
  labs(x = "Sample distribution",
       y = "Age (ka)") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = text_size),
    panel.grid = element_blank(),
    panel.border = element_rect(
      fill = NA,
      colour = "gray30",
      size = 0.1) )

#----------------------------------------------------------#
# 3. Figure 1 -----
#----------------------------------------------------------#
fig_map <- 
  Dataset_work %>%
  ggplot(aes(x = long, y = lat)) +
  borders(fill = "gray90", colour = "gray90") +
  coord_fixed(ylim = c(-55, 72), xlim = c(-160, 180)) +
  geom_point(aes(color = REGION),
             alpha = 1,
             size = 0.5,
             shape = 20) +
  scale_x_continuous(breaks = seq(-150, 150, 50), position = "top") +
  scale_y_continuous(breaks = seq(-80, 80, 20), position = "left") +
  theme_bw() +
  scale_color_manual(values = pallete_1) +
  labs(y = "Latitude",
       x = "Longitude") +
  guides(color = F) +
  theme(
    text = element_text(size = text_size),
    panel.grid = element_blank(),
    panel.border = element_rect(
      fill = NA,
      colour = "gray30",
      size = 0.1) )

#-------------------------------------------#
# 3.1 Env Data -----
#-------------------------------------------#
ngrip <-  read.csv("DATA/input/NGRIP.csv")
names(ngrip) <-  c("Age", "var")
ngrip$Age <-  ngrip$Age - 50
ngrip$var <-  ngrip$var - ngrip$var[1]

land_use <-  read.csv("DATA/input/LandUse.csv")

epica_CO2 <- 
  read.csv("DATA/input/EPICACO2.csv", header = T) %>%
  as_tibble()

names(epica_CO2) <-  c("Depth", "Age", "var", "Sigma")


#-------------------------------------------#
# 3.2 Env figures  -----
#-------------------------------------------#
fig_env_1 <- 
  land_use %>%
  ggplot() +
  geom_vline(
    xintercept = c(4.2e3, 8.2e3, 11.7e3, 17e3),
    color = "gray60",
    size = 0.1,
    lty = 1) +
  geom_ribbon(
    aes(
      ymin = 0,
      ymax = fit,
      y = fit,
      x = time),
    fill = "gray80",
    size = 0.1,
    alpha = 1) +
  geom_line(aes(y = fit, x = time), colour = "gray30", size = 0.1) +
  scale_x_continuous(
    trans = "reverse",
    breaks = seq(0, 20e3, 4e3),
    labels = seq(0, 20, 4)) +
  scale_y_continuous(breaks = seq(0, 1),
                     labels = c("None", "Widespread")) +
  coord_flip(xlim = c(20e3, -3e3),
             ylim = c(0, 1)) +
  labs(x = "Age (ka)",
       y = "intensive Agriculture") +
  theme_bw() +
  theme(
    text = element_text(size = text_size),
    panel.grid = element_blank(),
    panel.border = element_rect(
      fill = NA,
      colour = "gray30",
      size = 0.1) )

fig_env_2 <- 
  ngrip %>%
  ggplot(aes(x = Age, y = var)) +
  geom_vline(
    xintercept = c(4.2e3, 8.2e3, 11.7e3, 17e3),
    color = "gray60",
    size = 0.1,
    lty = 1) +
  geom_line(aes(y = var), size = 0.1, colour = "gray30") +
  geom_text(
    data = data.frame(
      Age = c(-2.5e3, 4.7e3, 8.7e3, 12.2e3, 17.5e3),
      label = c("LH", "MH", "EH", "LG", "FG") ),
    aes(label = label, y = -10),
    color = "gray30",
    size = 2) +
  scale_x_continuous(
    trans = "reverse",
    breaks = seq(0, 20e3, 4e3),
    labels = seq(0, 20, 4)) +
  scale_y_continuous(breaks = seq(-12, 3, 3), limits = c(-12, 3)) +
  coord_flip(xlim =  c(20e3, -3e3)) +
  labs(x = "Age (ka)",
       y = expression(paste("δ" ^ 18, "O", " (‰)", sep = ""))) +
  theme_bw() +
  theme(
    text = element_text(size = text_size),
    panel.grid = element_blank(),
    panel.border = element_rect(
      fill = NA,
      colour = "gray30",
      size = 0.1) )

fig_env_3 <- 
  epica_CO2 %>%
  ggplot(aes(x = Age, y = var)) +
  geom_vline(
    xintercept = c(4.2e3, 8.2e3, 11.7e3, 17e3),
    color = "gray60",
    size = 0.1,
    lty = 1) +
  geom_line(aes(y = var), size = 0.1, colour = "gray30") +
  scale_x_continuous(
    trans = "reverse",
    breaks = seq(0, 20e3, 4e3),
    labels = seq(0, 20, 4) ) +
  scale_y_continuous(breaks = seq(150, 300, 30)) +
  coord_flip(xlim = c(20e3, -3e3),
             ylim = c(180, 300)) +
  labs(x = "Age (ka)",
       y = expression(paste("CO"[2], " (ppmv)"))) +
  theme_bw() +
  theme(
    text = element_text(size = text_size),
    panel.grid = element_blank(),
    panel.border = element_rect(
      fill = NA,
      colour = "gray30",
      size = 0.1) )

#-------------------------------------------#
# 3.3 Construct Figure 1 -----
#-------------------------------------------#

fig_env <- 
  ggarrange(
    fig_env_1 + rremove("ylab"),
    fig_env_2 + rremove("ylab") + rremove("y.ticks") + rremove("y.text"),
    fig_env_3 + rremove("ylab") + rremove("y.ticks") + rremove("y.text"),
    fig_distribution_time + rremove("ylab") + rremove("y.ticks") +
      rremove("y.text"),
    nrow = 1,
    align = "h",
    labels = c("B", "C", "D", "E"),
    font.label = list(size = text_size) ) %>%
  annotate_figure(left = text_grob("Age (ka)", size = text_size, rot = 90))

xbp <-
  gghistogram(
    Dataset_work$long,
    fill = "lightblue1",
    binwidth = 5,
    size = 0.1) +
  theme_transparent()

ybp <-
  gghistogram(
    Dataset_work$lat,
    fill = "lightblue1",
    binwidth = 5,
    size = 0.1,) +
  ggpubr::rotate() +
  theme_transparent()

xbp_grob <-  ggplotGrob(xbp)
ybp_grob <-  ggplotGrob(ybp)

xmin <-  min(Dataset_work$long)
xmax <-  max(Dataset_work$long)
ymin <-  min(Dataset_work$lat)
ymax <-  max(Dataset_work$lat)

fig_map_a_fin <- 
  fig_map +
  annotation_custom(
    grob = xbp_grob,
    xmin = xmin,
    xmax = xmax,
    ymin = -71,
    ymax = -41) +
  annotation_custom(
    grob = ybp_grob,
    xmin = -187,
    xmax = -155,
    ymin = ymin,
    ymax = ymax)

FIGURE_01 <- 
  ggarrange(
    fig_map_a_fin,
    fig_env,
    nrow = 2,
    heights = c(1, 0.7),
    labels = c("A"),
    font.label = list(size = text_size) )

ggsave(
  "figures/FIGURE_01.pdf",
  FIGURE_01,
  width = 12,
  height = 9,
  units = "cm")
