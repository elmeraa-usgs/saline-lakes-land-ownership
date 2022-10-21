p1_targets <- list(
  # Load in land ownership and lake shape files
  # set projection for reading in all shape files
  tar_target(p1_proj,
             '+proj=aea +lat_0=35 +lon_0=-117 +lat_1=37.3 +lat_2=39.7 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
             ),
  # Read in watershed extents
  tar_target(p1_watershed_ext_sf,
             '1_fetch/in/Vizlab-saline_lakes_spatial_data/Saline_lake_watershed_extents/lake_watersheds.shp',
             format = 'file'),
  # st_read() watershed extents
  tar_target(p1_watershed_ex,
             st_read(p1_watershed_ext_sf) %>% st_transform(crs=p1_proj)),
  # Read in saline lake
  tar_target(p1_saline_lakes_sf,
             '1_fetch/in/Vizlab-saline_lakes_spatial_data/saline_lakes/saline_lakes.shp',
             format = 'file'),
  # st_read() saline lakes
  tar_target(p1_saline_lakes,
             st_read(p1_saline_lakes_sf) %>% st_transform(crs=p1_proj)),
  # Great Basin Dessert Band
  tar_target(p1_gbd_bnd_sf,
             '1_fetch/in/Vizlab-saline_lakes_spatial_data/GreatBasinDessertBnd_220721/GreatBasinDessertBnd.shp',
             format = 'file'),
  tar_target(p1_gbd_bnd,
             st_read(p1_gbd_bnd_sf) %>% st_transform(crs=p1_proj) %>%  ms_simplify()),
 #  PAD/land ownership
  tar_target(p1_pad_sf,
             '1_fetch/in/Shapefiles_220721/PADUS_3_0VA.shp',
             format='file'),
 tar_target(p1_pad,
            st_read(p1_pad_sf) %>% st_transform(crs=p1_proj)),
  # States
  tar_target(p1_states_sf,
             '1_fetch/in/states_shp/statesp010g.shp',
             format = 'file'),
 tar_target(p1_states,
            sf::st_read(p1_states_sf) %>% st_transform(crs=p1_proj) %>% ms_simplify())
 
)