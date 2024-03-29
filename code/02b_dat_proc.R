library(tidyverse)
library(sf)
library(lubridate)
library(rgdal)

prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

######
# processing data files for use with classify app

## 
# whole state data

# flw_pth <- 'ignore/SMR_NHDPlus.shp'
# shd_pth <- 'ignore/calwater_SWAMP3Code.shp'
flw_pth <- 'S:/Spatial_Data/NHDPlus/NHDPlus18/Hydrography/nhdflowline.shp'
shd_pth <- 'S:/Spatial_Data/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009.shp'
scr_pth <- 'input_data/00_csci_delta_formatted_median_updated_Nov2021.csv'
exp_pth <- 'ignore/comid_statewide.Rdata'
exp_pth
# stream expectations, named comid
load(file = exp_pth)

# csci scores
scrs <- read.csv(scr_pth, header = T, stringsAsFactors = F) %>% 
  rename(SampleID = sampleid, StationCode = stationcode, New_Lat = latitide, New_Long = longitude, CSCI = csci)
  dplyr::select(SampleID, StationCode, New_Lat, New_Long, COMID, SampleDate, CSCI) %>% 
  rename(
    csci = CSCI, 
    lat = New_Lat, 
    long = New_Long
  ) %>% 
  mutate(
    SampleDate = dmy(SampleDate), 
    COMID = as.character(COMID)
  )

# watersheds
shed <- readOGR(shd_pth) %>% 
  spTransform(CRS(prstr)) %>% 
  st_as_sf

# flowlines
spat <- readOGR(flw_pth) %>% 
  spTransform(CRS(prstr)) %>% 
  st_as_sf %>% 
  st_intersection(shed) %>% 
  select(COMID)  


##
# process separate spatial and score files for each watershed

# sheds to process, appended to file names
shds <- shed$SMC_Name

# process and save files for each shed
for(shd in shds){
  
  # counter
  cat(shd, which(shds %in% shd), 'of', length(shds), '\n')
  
  # filter watershed for intersect
  shd_tmp <- shed %>% 
    filter(SMC_Name %in% shd)
  
  # create spatial polyines from shed intersect, left_join with csci scrs
  sel <- st_covered_by(spat, shd_tmp, sparse = F) 
  spat_tmp <- spat %>% 
    filter(sel[, 1]) %>% 
    left_join(comid, by = 'COMID') %>% 
    select(COMID, matches('^full0'))
  
  # csci scores
  scrs_tmp <- scrs %>% 
    filter(COMID %in% spat_tmp$COMID)
  
  # assign unique names to scrs and spat
  scrs_shd <- paste0('scrs_', shd)
  spat_shd <- paste0('spat_', shd)
  assign(scrs_shd, scrs_tmp)
  assign(spat_shd, spat_tmp)
  
  # save unique scrs, spat
  save(list = scrs_shd, file = paste0('data/', scrs_shd, '.RData'))
  save(list = spat_shd, file = paste0('data/', spat_shd, '.RData'))
  
}