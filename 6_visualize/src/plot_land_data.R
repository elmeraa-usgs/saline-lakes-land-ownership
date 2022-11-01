######  Percent land charts ########

chart_greatBasinLand <- function(data) {
  
  data <- data |> mutate(MngGroup = case_when(MngGroup == "Native American areas" ~ "Tribal Land",  TRUE ~ MngGroup))  %>% # ensure tribal lands are denoted properly 
    filter(!str_detect(Label, "0%")) # get rid of 0% rows 

  ggplot(data = data, 
         aes(y= Proportion, x=reorder(MngGroup, Proportion, FUN = sum))) +
    geom_bar(stat = "identity", 
             aes(fill = MngGroup)) +
    geom_text(aes(label = Label), 
              position = position_dodge(width = 0.9),
              hjust= -0.01,
              size = 3.5) +
    geom_rect(ymin = -4, ymax = -1, xmin = seq(0.545, 11.545), xmax = seq(1.455, 12.455),
              aes(fill = MngGroup)) +
    coord_flip() +
    scale_fill_manual("",
                      values = c('Bureau of Land Management' = '#b3b691',
                                 'Unknown' = '#99A3A4',
                                 'Forest Service' = '#b3b691',
                                 'State entities' = '#045d3b',
                                 'Department of Defense' = '#b3b691',
                                 'U.S. Fish and Wildlife Service' = '#b3b691',
                                 'Tribal Land' = '#216a83',
                                 'Municipal entities' = "#045d3b",
                                 'National Park Service' = '#b3b691',
                                 'Federal - other' = '#b3b691',
                                 'Bureau of Reclamation' = '#b3b691',
                                 'NGO' = '#c89e3c')) +
    # scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    # viridis::scale_fill_viridis(discrete = TRUE, option="viridis", direction = -1, end = 0.8) + 
    scale_y_continuous(breaks = seq(0, 70, by = 10), 
                       limits = c(NA, 75), 
                       position  = 'right') + 
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 12)) +
    labs(title = "Percent of land within Great Basin",
         x = NULL, y = NULL) 
}


chart_lakeLand <- function(data, focal_lakes) {
  
  data_lake <- data  |>
    mutate(MngNm_D = case_when(MngNm_D == "American Indian Lands" ~ "Tribal Land",  TRUE ~ MngNm_D)) |> # ensure tribal lands are denoted properly
      filter(lk_w_st %in% focal_lakes) |>
  filter(!str_detect(Label, pattern = "^ 0%")) # get rid of 0% rows  
  
  ggplot(data = data_lake , 
         aes(y= Proportion, x=reorder(MngNm_D, Proportion))) +
    geom_bar(stat = "identity", 
             aes(fill=MngNm_D)) +
    geom_text(aes(label = Label), 
              position = position_stack(vjust = 1), 
              hjust=-0.05,
              size = 3) +
    scale_y_continuous(breaks = seq(0, 70, by = 10), 
                       limits = c(NA, 75), 
                       position  = 'right') + 
    #scale_fill_manual(values = manualcolors) +
    # scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    # scale_fill_scico_d(palette = 'cividis', direction = -1, end = 0.8) +
    geom_rect(data = data_lake |> distinct(Mng_Level, MngNm_D, Proportion),
              aes(fill = MngNm_D),
              ymin = -3, ymax = -1,
              xmin = seq(0.545, as.numeric(paste(length(unique(data_lake$MngNm_D)))) - 0.455), xmax = seq(1.455, as.numeric(paste(length(unique(data_lake$MngNm_D)))) + 0.455),
              position = position_dodge2(reverse = TRUE)) +
    scale_fill_manual("",
                      values = c('Bureau of Land Management' = '#b3b691',
                                 'Bureau of Reclamation' = '#b3b691', 
                                 'Unknown' = '#99A3A4',
                                 'Private' = '#99A3A4',
                                 'Forest Service' = '#b3b691',
                                 'State Department of Natural Resources' = '#045d3b',
                                 'Department of Defense' = '#b3b691',
                                 'State Land Board' = '#045d3b', 
                                 'State Fish and Wildlife' = '#045d3b', 
                                 'U.S. Fish and Wildlife Service' = '#b3b691',
                                 'National Park Service' = '#b3b691',
                                 'State Park and Recreation' = '#045d3b',
                                 'State Department of Land' = '#045d3b', 
                                 'Natural Resources Conservation Service' = '#b3b691',
                                 'City Land' = '#045d3b', 
                                 'Other or Unknown Local Government' = '#045d3b',
                                 'Other or Unknown State Land'  = '#045d3b',
                                 'County Land' = '#045d3b',
                                 'Tribal Land' = '#216a83',
                                 'Non-Governmental Organization' = '#c89e3c',
                                 'State Department of Conservation' = '#045d3b',
                                 'Army Corps of Engineers' = '#b3b691'))  +
    coord_flip() +
    theme_minimal() +
    labs(title= "Percent of land within lake watershed",
         x=NULL, y = NULL) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5))
}

chart_basinFederal <- function(data){
  
  data <- data |> mutate(MngGroup = case_when(MngGroup == "Native American areas" ~ "Tribal Land",  TRUE ~ MngGroup))  %>% # ensure tribal lands are denoted properly 
    filter(Mng_Level %in% c("Federal")) # only displaying federal lands 
   
  ggplot(data = data, 
         aes(y= Proportion, x=reorder(MngGroup, Proportion, FUN = sum))) +
    geom_bar(stat = "identity", 
             aes(fill = MngGroup)) +
    geom_text(aes(label = Label), 
              position = position_dodge(width = 0.9),
              hjust= -0.01,
              size = 3.5) +
    geom_rect(ymin = -4, ymax = -1, xmin = seq(0.545, 5.545), xmax = seq(1.455, 6.455),
              aes(fill = MngGroup)) +
    coord_flip() +
    #scale_fill_manual(values = newcolors) + 
    # scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    # viridis::scale_fill_viridis(discrete = TRUE, option="mako", direction = -1, begin = 0.3, end = 0.9) + 
    scale_fill_manual("",
                      values = c('Bureau of Land Management' = '#b3b691',
                                 'Forest Service' = '#045d3b',
                                 'Department of Defense' = '#216a83',
                                 'U.S. Fish and Wildlife Service' = '#c89e3c',
                                 'National Park Service' = '#7A5C12',
                                 'Bureau of Reclamation' = '#b65616')) + 
    scale_y_continuous(breaks = seq(0, 70, by = 10), 
                       limits = c(NA, 75), 
                       position  = 'right') + 
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.250, size = 12)) +
    labs(title = "Percent of federal land within Great Basin",
         x = NULL, y = NULL) 
  
}

chart_lakeLandFed <- function(data, focal_lakes) {
  
  data <- data |> mutate(MngNm_D = case_when(MngNm_D == "American Indian Lands" ~ "Tribal Land",  TRUE ~ MngNm_D)) %>% # ensure tribal lands are denoted properly 
    filter(Mng_Level %in% c("Federal")) |> # only displaying federal lands 
    filter(!str_detect(Label, pattern = "^ 0%")) # remove labels with 0%
  
  data_lake <-  data |> filter(lk_w_st %in% focal_lakes)
  
  ggplot(data = data_lake , 
         aes(y= Proportion, x=reorder(MngNm_D, Proportion))) +
    geom_bar(stat = "identity", 
             aes(fill=MngGroup)) +
    geom_text(aes(label = Label), 
              position = position_stack(vjust = 1), 
              hjust=-0.03,
              size = 3) +
    scale_y_continuous(breaks = seq(0, 70, by = 10), 
                       limits = c(NA, 75), 
                       position  = 'right') + 
    #scale_fill_manual(values = manualcolors) +
    # scale_fill_scico_d(palette = 'cividis', direction = -1, end = 0.8) +
    geom_rect(data = data_lake |> distinct(Mng_Level, MngNm_D, Proportion),
              aes(fill = MngGroup),
              ymin = -3, ymax = -1,
              xmin = seq(0.545, as.numeric(paste(nrow(data_lake) - 0.455))), xmax = seq(1.455, as.numeric(paste(nrow(data_lake) + 0.455))),
              position = position_dodge2(reverse = TRUE)) +
    # scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) + 
    scale_fill_manual("",
                      values = c('Bureau of Land Management' = '#b3b691',
                                 'Forest Service' = '#045d3b',
                                 'Department of Defense' = '#216a83',
                                 'U.S. Fish and Wildlife Service' = '#c89e3c',
                                 'National Park Service' = '#7A5C12',
                                 'Bureau of Reclamation' = '#b65616',
                                 'Federal - other' = '#b81469')) + 
    coord_flip() +
    theme_minimal() +
    labs(title= "Percent of federal land within lake watershed",
         x=NULL, y = NULL) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.04, size = 12),
          plot.subtitle = element_text(hjust = 0.5))
}

## MAPS 
map_greatBasin <- function(data, data_gbd_outline, join, zoom) {
  
  basemap <- maptiles::get_tiles(x = data, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = zoom, forceDownload = T)
  

 data_join = data  |> left_join(join)
 
ggm1 = ggplot(data_join) +
    geom_spatraster_rgb(data = basemap) +
    geom_sf(
      aes(fill = Mng_Level),
      color = NA,
      inherit.aes = FALSE) +
  geom_sf(data = data_gbd_outline %>% ms_simplify(),
          fill = NA, 
          color = 'black',
          inherit.aes = FALSE,
          alpha = 0.5) + 
    coord_sf() + 
    # scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    # viridis::scale_fill_viridis(discrete = TRUE, option="viridis", direction = -1, end = 0.8) + 
    #scale_fill_manual(values = col_pal, breaks = breaks, labels = labels) +
    scale_fill_manual("",
                      values = c('Federal' = '#b3b691',
                                 'Private or Unknown' = '#788687',
                                 'Regional/State/Local' = '#045d3b',
                                 'Tribal Land' = '#216a83',
                                 'NGO' = '#c89e3c')) +
    labs(fill='') +
    scale_alpha(range = c(0.5, 1)) +
    guides() +
    theme_void() +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          plot.background = element_rect(fill = 'white', color = NA)) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location="br",style = "ticks", line_width = 0.8, width_hint =0.4, pad_x = unit(0.5, "cm"), tick_height = 0.0)
  
}


map_lake <- function(data, focal_lakes,data_watershed_outline, join, zoom) {
  
lakesData <- data |>
  filter(lk_w_st %in% focal_lakes)

basemap <- get_tiles(x = lakesData, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = zoom, forceDownload = T)

ggm2 = lakesData |>
  left_join(join) |>
  ggplot() +
  geom_spatraster_rgb(data = basemap |> terra::crop(lakesData)) +
  geom_sf(
    aes(fill = Mng_Level), 
    color = NA, size = 0.2) + 
  geom_sf(data = data_watershed_outline |> filter(lk_w_st %in% focal_lakes) %>% ms_simplify(),
          fill = NA, 
          color = 'black',
          inherit.aes = FALSE,
          alpha = 0.5) + 
  #geom_sf(data = saline_lakes |> filter(lk_w_st %in% focal_lakes), 
  #       fill = '#b2d8d8') +
  coord_sf() + 
  # scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
  # viridis::scale_fill_viridis(discrete = TRUE, option="viridis", direction = -1, end = 0.8) + 
  scale_fill_manual("",
                    values = c('Federal' = '#b3b691',
                               'Private or Unknown' = '#788687',
                               'Regional/State/Local' = '#045d3b',
                               'Tribal Land' = '#216a83',
                               'NGO' = '#c89e3c')) +
  labs(fill='Management Type') +
  guides(color="none") +
  theme_void() +
  scale_alpha(range = c(0.5, 1)) +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5))  +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location="br",style = "ticks", line_width = 0.8, width_hint =0.4, pad_x = unit(0.5, "cm"), tick_height = 0.0)
}



map_basin_inset <- function(data_gbd, data_lakes, proj) {
  
  
  data_lakes_centroids <- st_centroid(data_lakes) %>% st_transform(crs=proj)
  
  ggm3 = ggplot() +
    geom_sf(data = data_gbd %>%  ms_simplify(),
            fill = '#616A6B',
            color = 'NA',
            inherit.aes = FALSE,
            alpha = 0.5) +
    geom_sf(data = data_lakes_centroids,
            fill = NA,
            color = 'black',
            alpha = 0.5, 
            size = 2, 
            inherit.aes = FALSE) +
    # geom_sf(data = data_lakes,
    #         fill = "#D6EAF8", color = 'black', alpha=0.5) +
    coord_sf() +
    theme(axis.text.y   = element_blank(),
          axis.text.x   = element_blank(),
          axis.title.y  = element_blank(),
          axis.title.x  = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.border = element_rect(color = "black", fill=NA, size=0.75)
    )
  
}


map_lake_inset <- function(data_gbd, data_lakes, data_watershed, focal_lakes, proj) {
  
  
  lake_cent <- st_centroid(data_lakes |> filter(lk_w_st %in% focal_lakes) %>% st_transform(crs=proj))
  
  ggm4 = ggplot() +
    # geom_sf(data = data_gbd,
    #         fill = 'white',
    #         color = 'black',
    #         inherit.aes = FALSE,
    #         alpha = 0.5) +
    geom_sf(data = data_watershed |> filter(lk_w_st %in% focal_lakes) %>%  ms_simplify(),
            fill = '#616A6B',
            color = NA,
            inherit.aes = FALSE,
            alpha = 0.5)+
    # geom_sf(data = lake_cent,
    #         fill = NA,
    #         color = '#515A5A',
    #         alpha = 0.5, 
    #         size = 3, 
    #         inherit.aes = FALSE) +
    geom_sf(data = data_lakes |> filter(lk_w_st %in% focal_lakes) %>%  ms_simplify(),
            fill = "black", color = NA) + 
    coord_sf() +
    theme(axis.text.y   = element_blank(),
          axis.text.x   = element_blank(),
          axis.title.y  = element_blank(),
          axis.title.x  = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.border = element_rect(color = "black", fill=NA, size=0.75)
    )

  
}


map_greatBasinFederal <- function(data,data_gbd_outline, join, zoom) {
  
  basemap <- maptiles::get_tiles(x = data, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = zoom, forceDownload = T)
  
  ggm1 = data |>
    left_join(join) |>
    filter(Mng_Level %in% c("Federal")) %>%  
    ggplot() +
    geom_spatraster_rgb(data = basemap) +
    geom_sf(
      aes(fill = MngGroup),
      color = NA,
      inherit.aes = FALSE) +
    geom_sf(data = data_gbd_outline %>% ms_simplify(),
            fill = NA, 
            color = 'black',
            inherit.aes = FALSE,
            alpha = 0.5) + 
    # scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    # viridis::scale_fill_viridis(discrete = TRUE, option="mako", direction = -1, begin = 0.3, end = 0.9) + 
    scale_fill_manual("",
                      values = c('Bureau of Land Management' = '#b3b691',
                                 'Forest Service' = '#045d3b',
                                 'Department of Defense' = '#216a83',
                                 'U.S. Fish and Wildlife Service' = '#c89e3c',
                                 'National Park Service' = '#7A5C12',
                                 'Bureau of Reclamation' = '#b65616')) + 
    labs(fill='') +
    scale_alpha(range = c(0.5, 1)) +
    guides() +
    theme_void() +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          plot.background = element_rect(fill = 'white', color = NA)) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
  annotation_scale(location="br",style = "ticks", line_width = 0.8, width_hint =0.4, pad_x = unit(0.5, "cm"), tick_height = 0.0)
  
}


map_lakeFederal <- function(data, focal_lakes,data_watershed_outline, join, zoom) {
  
  lakesData <- data |> 
    filter(lk_w_st %in% focal_lakes)  
  
  basemap <- get_tiles(x = lakesData, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = zoom, forceDownload = T)
  
  ggm2 = lakesData |>
    left_join(join) |>
    filter(Mng_Level %in% c("Federal")) |>
    ggplot() +
    geom_spatraster_rgb(data = basemap |> terra::crop(lakesData)) +
    geom_sf(
      aes(fill = MngGroup), 
      color = NA, size = 0.2) + 
    geom_sf(data = data_watershed_outline |> filter(lk_w_st %in% focal_lakes) %>% ms_simplify(),
            fill = NA, 
            color = 'black',
            inherit.aes = FALSE,
            alpha = 0.5) + 
    #geom_sf(data = saline_lakes |> filter(lk_w_st %in% focal_lakes), 
    #       fill = '#b2d8d8') +
    coord_sf() + 
    # scale_fill_scico_d(palette = 'batlow', direction = -1, end = 0.8) +
    # viridis::scale_fill_viridis(discrete = TRUE, option="viridis", direction = -1, end = 0.8) + 
    scale_fill_manual("",
                      values = c('Bureau of Land Management' = '#b3b691',
                                 'Forest Service' = '#045d3b',
                                 'Department of Defense' = '#216a83',
                                 'U.S. Fish and Wildlife Service' = '#c89e3c',
                                 'National Park Service' = '#7A5C12',
                                 'Bureau of Reclamation' = '#b65616',
                                 'Federal - other' = '#b81469')) + 
    labs(fill='Management Type') +
    guides(color="none") +
    theme_void() +
    scale_alpha(range = c(0.5, 1)) +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5))  +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location="br",style = "ticks", line_width = 0.8, width_hint =0.4, pad_x = unit(0.5, "cm"), tick_height = 0.0)
}
