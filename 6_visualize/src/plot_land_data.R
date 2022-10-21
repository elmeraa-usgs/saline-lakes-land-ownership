######  Percent land charts ########

chart_greatBasinLand <- function(data) {
  
  ggplot(data = data, 
         aes(y= Proportion, x=reorder(MngGroup, Proportion, FUN = sum))) +
    geom_bar(stat = "identity", 
             aes(fill = MngGroup)) +
    geom_text(aes(label = Label), 
              position = position_dodge(width = 0.9),
              hjust= -0.01,
              size = 3.5) +
    geom_rect(ymin = -4, ymax = -1, xmin = seq(0.545, 15.545), xmax = seq(1.455, 16.455),
              aes(fill = MngGroup)) +
    coord_flip() +
    #scale_fill_manual(values = newcolors) + 
    scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(NA, 60), position  = 'right') +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size=14)) +
    labs(title = "Percent of land within Great Basin",
         x = NULL, y = NULL) 
}


chart_lakeLand <- function(data, focal_lakes) {
  
  data_lake <-  data |> filter(lk_w_st %in% focal_lakes)
  
  ggplot(data = data_lake , 
         aes(y= Proportion, x=reorder(MngNm_D, Proportion))) +
    geom_bar(stat = "identity", 
             aes(fill=MngGroup)) +
    geom_text(aes(label = Label), 
              position = position_stack(vjust = 1), 
              hjust=-0.05,
              size = 3) +
    scale_y_continuous(breaks = seq(0, max(data_lake$Proportion)+ 10, by = 10), 
                       limits = c(NA, max(data_lake$Proportion) + 5), 
                       position  = 'right') + 
    #scale_fill_manual(values = manualcolors) +
    scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    geom_rect(data = data_lake |> distinct(Mng_Level, MngNm_D, Proportion),
              aes(fill = MngGroup),
              ymin = -3, ymax = -1,
              xmin = seq(0.545, as.numeric(paste(nrow(data_lake) - 0.455))), xmax = seq(1.455, as.numeric(paste(nrow(data_lake) + 0.455))),
              position = position_dodge2(reverse = TRUE)) +
    coord_flip() +
    theme_minimal() +
    labs(title= "Percent of land within lake watershed",
         x=NULL, y = NULL) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

map_greatBasin <- function(data, join, zoom) {
  
  basemap <- maptiles::get_tiles(x = data, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = zoom, forceDownload = T)
  
  data |>
    left_join(join) |>
    ggplot() +
    geom_spatraster_rgb(data = basemap) +
    geom_sf(
      aes(fill = MngGroup),
      color = NA,
      inherit.aes = FALSE) +
    coord_sf() + 
    # scale_color_manual(values = manualcolors) +
    # scale_fill_manual(values = newcolors) +
    scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    labs(fill='') +
    scale_alpha(range = c(0.5, 1)) +
    guides() +
    theme_void() +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          plot.background = element_rect(fill = 'white', color = NA)) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering)
}


map_lake <- function(data, focal_lakes, join, zoom) {
  
lakesData <- data |>
  filter(lk_w_st %in% focal_lakes)

basemap <- get_tiles(x = lakesData, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = zoom, forceDownload = T)

lakesData |>
  left_join(join) |>
  ggplot() +
  geom_spatraster_rgb(data = basemap |> terra::crop(lakesData)) +
  geom_sf(
    aes(fill = MngGroup), 
    color = NA, size = 0.2) + 
  #geom_sf(data = saline_lakes |> filter(lk_w_st %in% focal_lakes), 
  #       fill = '#b2d8d8') +
  coord_sf() + 
  # scale_fill_manual(values = colPal4) +
  scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
  labs(fill='Management Type',
       title = sub("\\,", ", ", focal_lakes)) +
  guides(color="none") +
  theme_void() +
  scale_alpha(range = c(0.5, 1)) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)
}