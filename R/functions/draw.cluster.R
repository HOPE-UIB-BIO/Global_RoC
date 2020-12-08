draw.cluster <-  function(region, cluster_method, distance) {
  
  data_w <- Dataset_work %>%
    dplyr::filter(REGION == region) %>%
    arrange(dataset.id) %>%
    na.omit()
  
  clusters <- data_w %>%
    arrange(dataset.id) %>%
    mutate(
      MAT = scale(MAT),
      T.var = scale(T.var),
      Perc.dry = scale(Perc.dry),
      Perc.var = scale(Perc.var),
      lat = scale(lat),
      long = scale(long),
      elev = scale(elev)) %>%
    dplyr::select(MAT
                  , T.var
                  , Perc.dry
                  , Perc.var
                  , lat
                  , long
                  , elev) %>%
    NbClust(
      .,
      min.nc = 3,
      max.nc = min(c(nrow(data_w) - 1, 10)),
      method = cluster_method,
      distance = distance
    )
  
  data_w <-  
    data_w %>%
    arrange(dataset.id) %>%
    mutate(cluster = as.factor(clusters$Best.partition))
  
  data_w$cluster <- with(data_w, reorder(cluster, -lat, mean, ))
  
  
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
  
  
  return(
    list(
      data_sites = data_w,
      plot = p1))
  
}