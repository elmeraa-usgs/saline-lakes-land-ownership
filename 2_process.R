source('2_process/src/process_spatial.R')


p2_targets <- list(
  # Processing PAD data to reduce variables
  tar_target(p2_pad_clean,
             pad_data(p1_pad)),
  
  # intersect pad to great basin, saline lakes, and watershed extent
  tar_target(p2_gbd_pad_int,
             pad_int(data = p1_gbd_bnd, pad_data = p2_pad_clean)),
  tar_target(p2_saline_lakes_pad_int,
             pad_int(data = p1_saline_lakes, pad_data = p2_pad_clean)),
  tar_target(p2_watershed_ext_pad_int,
             pad_int(data = p1_watershed_ex, pad_data = p2_pad_clean)),
  
  # group data by Mng_Typ and calculate the total land area per Mng_Typ
  tar_target(p2_gbd_ByMng,
             gbdByMng(p2_gbd_pad_int)),
  tar_target(p2_watershedExt_ByMng,
             watershedExtByMng(p2_watershed_ext_pad_int)),
  
  # group data by MngNm_D and calculate the total land area per MngNm_D (more detailed Management Types)
  tar_target(p2_gbd_ByMngNm,
             gbdByMngNm(p2_gbd_pad_int)),
  tar_target(p2_watershedExt_ByMngNm,
             watershedExtByMngNm(p2_watershed_ext_pad_int)),
  
  # Group some of the Management Type names to reduce colors needed 
  tar_target(p2_gbd_ByMngNm_reduce,
             reduceMng(p2_gbd_ByMngNm)),
  tar_target(p2_watershedExt_ByMngNm_reduce,
             reduceMng(p2_watershedExt_ByMngNm)),
  
  # Grouping my management level for plotting Great Basin map 
  tar_target(p2_gbd_pal,
             gbd_pal(p2_gbd_ByMngNm_reduce)),
  
  # For percent of land charts, add proportion and label columns
  # Great Basin level 
  tar_target(p2_gbdByMngNm_grp_pArea,
             gbdByMngNm_grp_pArea(p2_gbd_ByMngNm_reduce)),
  
  # provide a target for focal lakes to filter  
  tar_target(p2_focal_lakes,
             tibble(lakes = sort(unique(p2_watershedExt_ByMngNm_reduce$lk_w_st)))),
  
# # watershed lake level - get area by management type for each lake 
  tar_target(p2_watershedExtByMngNm_lakeArea,
             watershedExtByMngNm_lakeArea(p2_watershedExt_ByMngNm_reduce)),

# watershed lake level - sum area by lake and management name 
tar_target(p2_watershedExtByMngNm_sum,
           watershedExtByMngNm_sum(p2_watershedExt_ByMngNm_reduce)),
  
  
  # # For percent of land charts, add proportion and label columns 
  # # watershed lake level - Pyramid Lake, NV and Mono Lake, CA
  tar_target(p2_watershedExtByMngNm_grp_pArea, 
             watershedExtByMngNm_pArea(lake_area = p2_watershedExtByMngNm_lakeArea, data =p2_watershedExt_ByMngNm_reduce, 
                                       focal_lakes =c("Pyramid Lake,NV", 'Winnemucca Lake,NV')))

# # add basemap
# tar_target(p2_basemap_greatBasin, 
#            basemap(data = p2_gbd_ByMngNm_reduce , zoom =7))

)

