# mutate Management Type of PAD data to reduce variables
pad_data <- function(data){
  data |>
    select(OBJECTI, Mng_Typ, Mang_Nm, Unit_Nm, GIS_Acr, MngTp_D, MngNm_D, MngNm_L, BndryNm, ST_Name) |>
    mutate(Mng_Level = case_when(
      Mng_Typ == 'FED' ~ 'Federal',
      Mng_Typ %in% c('LOC', 'STAT', 'DIST', 'JNT') ~ 'Regional/State/Local',
      Mng_Typ %in% c('PVT','UNK') ~ 'Private or Unknown',
      Mng_Typ == 'NGO' ~ 'NGO',
      Mng_Typ == 'TRIB' ~ 'Tribal Land'))
}

# Intersect `pad` to each of `p1_gbd_bnd, `p1_saline_lakes`, and `p1_watershed_ex`
pad_int <- function(data, pad_data){
  data |> 
    st_intersection(pad_data) |>
    mutate(area = st_area(geometry))
}

# group data by Mng_Typ and calculate the total land area per Mng_Typ for Great Desert Basin 
gbdByMng <- function(data){
  data |> 
    group_by(Mng_Typ) |>
    summarise(areaMng_Typ = as.numeric(sum(area))) |>
    arrange(desc(areaMng_Typ)) |>
    ms_simplify()
}

# group data by Mng_Typ and calculate the total land area per Mng_Typ and Lake for Watershed Extent
watershedExtByMng <- function(data){
  data |> 
    group_by(Mng_Typ, lk_w_st) %>%
    summarise(areaMng_Typ = as.numeric(sum(area)))|>
    ms_simplify()
}

# group data by MngNm_D and calculate the total land area per MngNm_D (more detailed Management Types)
# Great Basin Level
gbdByMngNm <- function(data){
  data |> 
    group_by(Mng_Level, MngNm_D) |>
    summarise(areaMngNm_Typ = as.numeric(sum(area))) |>
    ms_simplify()
}

# Watershed Extent Level
watershedExtByMngNm <- function(data){
  data |> 
    group_by(Mng_Level, MngNm_D, lk_w_st) |>
    summarise(areaMngNm_Typ = as.numeric(sum(area))) |>
    ms_simplify()
}

# Group some of the Management Type names to reduce colors needed 
reduceMng <- function(data, out_file){
  out <- data |>
    mutate(MngGroup = case_when(
      MngNm_D == "American Indian Lands" ~ "Tribal land", 
      MngNm_D == "Agricultural Research Service" ~ "Federal - other",
      MngNm_D == "Army Corps of Engineers" ~ "Federal - other", 
      MngNm_D == "Department of Energy" ~ "Federal - other",
      MngNm_D == "Natural Resources Conservation Service" ~ "Federal - other", 
      MngNm_D == "State Department of Conservation" ~ "State entities",
      MngNm_D == "Other or Unknown State Land" ~ "State entities", 
      MngNm_D == "State Park and Recreation" ~ "State entities",
      MngNm_D == "State Department of Land" ~ "State entities",
      MngNm_D == "State Fish and Wildlife" ~ "State entities", 
      MngNm_D == "State Department of Natural Resources" ~ "State entities", 
      MngNm_D == "State Land Board" ~ "State entities", 
      MngNm_D == "Regional Agency Land" ~ "Regional entites",
      MngNm_D == "County Land" ~ "Municipal entities",
      MngNm_D == "City Land" ~ "Municipal entities", 
      MngNm_D == "Non-Governmental Organization" ~ "NGO",
      TRUE ~ MngNm_D
    ))
  
  saveRDS(out, out_file)
  return(out_file)
}

# Get basemap
basemap <- function(data, zoom){
  maptiles::get_tiles(x = data, provider = "CartoDB.PositronNoLabels", crop = T, verbose = T, zoom = zoom, forceDownload = T)
}

# Grouping my management level 
gbd_pal <- function(data, out_file){
  out <- data |> 
    st_drop_geometry() |>
    group_by(Mng_Level) |>
    arrange(Mng_Level, MngGroup) |>
    mutate(ord = row_number())
  
  saveRDS(out, out_file)
  return(out_file)  
  
}

# Percent of land charts, add proportion and label columns 
# Great Basin Level 
gbdByMngNm_grp_pArea <- function(data, out_file){
 out <-  data |> 
    st_drop_geometry() |>
    group_by(MngGroup) %>% 
    summarize(areaMngNm_Typ = sum(areaMngNm_Typ)) |>
    mutate(GB_area = sum(areaMngNm_Typ),
           Proportion = as.numeric(100 * (areaMngNm_Typ /GB_area)),
           Label = sprintf(" %s%%", round(Proportion, 1))) |>
    arrange(Proportion) |> 
    left_join(data)
 
 saveRDS(out, out_file)
 return(out_file) 
}

# watershed lake level - get area by management type for each lake 
watershedExtByMngNm_lakeArea <- function(data){
  data |>
    st_drop_geometry() |>
    group_by(lk_w_st) |> 
    summarise(areaMngNm_Typ = sum(areaMngNm_Typ)) 
}

# watershed lake level - sum area by lake and management name 
watershedExtByMngNm_sum <-function(data){
  data |>  
    group_by(MngNm_D, lk_w_st) |> 
  summarize(area =sum(areaMngNm_Typ))
}

# watershed lake level 
watershedExtByMngNm_pArea <- function(lake_area, data, out_file){
  
  watershedExtByMngNm_pArea <- merge(x = lake_area, y = data, by = "lk_w_st") |>
    # filter(lk_w_st %in% focal_lakes) |>  # filter for focal lakes of interest 
    group_by(Mng_Level, MngGroup, MngNm_D, lk_w_st) |>
    summarize(areaMngNm_Typ.y = sum(areaMngNm_Typ.y),
              areaMngNm_Typ.x = sum(areaMngNm_Typ.x)) |>
    mutate(Proportion = as.numeric(100 * (areaMngNm_Typ.y /areaMngNm_Typ.x)),
           Label = sprintf(" %s%%", round(Proportion, 1))) |>
    arrange(-Proportion) 
  
  saveRDS(watershedExtByMngNm_pArea, out_file)
  return(out_file) 
  
}

