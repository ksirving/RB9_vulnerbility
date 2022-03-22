library(tidyverse)
library(readxl)
library(raster)
library(maptools)
library(rgdal)
library(sf)
library(rmapshaper)
library(proj4shortcut)
library(leaflet)
library(RColorBrewer)
library(doParallel)
library(foreach)
library(randomForest)

source('R/funcs.R')
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


# # csci data, regional
csci_raw <- readRDS("output_data/00_csci_comids.rds")

## select only needed columns. remove ffm
csci_raw <- csci_raw %>%
  dplyr::select(stationcode:sampledate, csci:longitude, comid, WatershedArea_Mi2)

## save 
save(csci_raw, file = 'output_data/03_csci_raw.RData', compress = 'xz')


# Format regions data -----------------------------------------------------


load(file = 'data/calipsa.RData') ## regions - not neccesary but maybe stream class?
##
# psa labels by centroid, deserts modoc has two
psalab <- calipsa %>%
  as('Spatial') %>%
  rmapshaper::ms_explode() %>%
  st_as_sf %>%
  mutate(
    AREA = st_area(.),
    AREA = gsub('\\sm\\^2$', '', AREA),
    AREA = as.numeric(AREA)
  ) %>%
  group_by(PSA6) %>%
  top_n(2, AREA) %>%
  ungroup %>%
  filter(!(PSA6 == 'South Coast' & AREA < 1e10)) %>%
  filter(!(PSA6 == 'Chaparral' & AREA < 1e10)) %>%
  st_centroid

psalab <- psalab %>%
  st_coordinates %>%
  data.frame %>%
  rename(
    long = X,
    lat = Y
  ) %>%
  mutate(
    Region = psalab$PSA6,
    Region = factor(Region,
                    levels = c('Central Valley', 'Chaparral', 'Deserts Modoc', 'North Coast', 'Sierra Nevada', 'South Coast'),
                    labels = c('CV', 'CH', 'DM', 'NC', 'SN', 'SC')
    )
  )

save(psalab, file = 'output_data/03_psalab.RData', compress = 'xz')
### the section arranges regions, might not need it!!!!!


# Join predictions to reaches ----------------------------------------------------------

# cali NHD simplify, fortify, and save

load(file = 'output_data/02_comid_prd.RData')

## rename comid to match shp files
comid_prd <- comid_prd %>%
  rename(COMID = comid)

# RB9 reaches really simplified
calinhd <- readOGR('/Users/katieirving/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
unique(calinhd$COMID)
unique(comid_prd$COMID)

# comid predictions to join with calinhd
comid_prd <- comid_prd %>%
  dplyr::select(COMID, core0.50) %>% 
  na.omit

dim(comid_prd)  ## 68663 sites

# fortified calinhd, joind with comid pred
nhdplo <- calinhd %>%
  filter(COMID %in% unique(comid_prd$COMID)) %>%
  dplyr::select(COMID) %>% ## 46
  as('Spatial')

comidid <- nhdplo@data %>%
  dplyr::select(COMID) %>%
  rownames_to_column('id')

nhdplo <- nhdplo %>%
  fortify %>%
  left_join(comidid, by = 'id') %>%
  inner_join(comid_prd, by = 'COMID')

nhdplo

save(nhdplo, file = 'output_data/03_nhdplo.RData', compress = 'xz')


# Format stream class -----------------------------------------------------

# cali nhd stream classes, simplified, fortified, and saved

load(file = 'output_data/02_comid_prd.RData')
comid_prd
## rename comid to match shp files
comid_prd <- comid_prd %>%
  rename(COMID = comid)

# comid predictions to join with calinhd
comid_prd <- comid_prd %>% 
  dplyr::select(COMID, matches('^core|^COMID$'))

# all cali hydro, really simplified
calinhd <- readOGR('/Users/katieirving/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHD_reaches_RB9_castreamclassification.shp') %>% 
  spTransform(prj) %>% 
  st_as_sf %>% 
  st_simplify(dTolerance = 0.5, preserveTopology = T) %>% 
  filter(COMID %in% unique(comid_prd$COMID)) %>% 
  dplyr::select(COMID, CLASS) %>%
  inner_join(comid_prd, by = 'COMID')

calinhd
# get biological condition expectations
cls <- getcls2(calinhd, thrsh = 0.79, tails = 0.1, modls = 'core')
cls
calicls <- calinhd %>% 
  left_join(cls, by = 'COMID')

caliclsid <- calicls %>% 
  as('Spatial') %>% 
  .@data %>% 
  dplyr::select(COMID, strcls, CLASS) %>% 
  rownames_to_column('id') 

class(calicls)
head(calicls)

classes <- calicls %>%
  select(strcls, geometry)
plot(classes)

caliclsplo <- calicls %>% 
  as('Spatial') %>% 
  fortify %>% 
  left_join(caliclsid, by = 'id') 

# library(tidyverse)

# calicls_coord <- calicls %>%
#   mutate(lat = unlist(map(calicls$geometry,1)),
#          long = unlist(map(calicls$geometry,2)))
# 
# separated_coord

# write_csv(calicls, "GIS/03_constrained_class.csv")

save(calicls, file = 'output_data/03_calicls.RData', compress = 'xz') ## predicted percentiles with constrained class and stream class
save(caliclsplo, file = 'output_data/03_caliclsplo.RData', compress = 'xz') ### lat lon constrained class and group



# CSCI site expectations --------------------------------------------------

# all cali CSCI site expectations
## predicted
load(file = 'output_data/02_comid_prd.RData')

## rename comid to match shp files
comid_prd <- comid_prd %>%
  rename(COMID = comid)

# all comid performance
# data(csci_comid)
load(file = 'output_data/02_csci_comid.RData')

## rename cols to match shp files
csci_comid <- csci_comid %>%
  rename(COMID = comid,StationCode = stationcode, CSCI = csci, SampleDate = sampledate, 
         New_Lat = latitude, New_Long = longitude )

head(csci_comid)

# all cali hydro, really simplified
calinhd <- readOGR('/Users/katieirving/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>% 
  spTransform(prj) %>% 
  st_as_sf %>% 
  st_simplify(dTolerance = 0.5, preserveTopology = T) %>% 
  filter(COMID %in% unique(comid_prd$COMID)) %>% 
  dplyr::select(COMID) %>%
  inner_join(comid_prd, by = 'COMID')

# format csci data for site_exp function, takes averages of repeats at each site
csci_comid <- csci_comid %>% 
  dplyr::select(COMID, StationCode, CSCI, SampleDate, New_Lat, New_Long) %>% 
  group_by(COMID, StationCode) %>% 
  summarise(
    csci = mean(CSCI, na.rm = T), 
    lat = mean(New_Lat, na.rm = T),
    long = mean(New_Long, na.rm = T)
  ) %>% 
  ungroup

# get site expectations
caliexp <- site_exp(calinhd, csci_comid, thrsh = 0.79, tails = 0.1, modls = 'core')

save(caliexp, file = 'output_data/03_caliexp.RData', compress = 'xz') ## expected constarined class


# Stream Length -----------------------------------------------------------

# get stream length in each stream class by PSA

load(file = 'output_data/03_calicls.RData')
load(file = 'output_data/03_caliexp.RData')
load(file = 'output_data/02_csci_comid.RData')
# load(file = 'output_data/03_calipsa.RData') ## regions = not needed
calicls
calipsa
strclslen <- calicls %>% 
  dplyr::select(COMID, strcls) %>% 
  st_intersection(calipsa)
head(strclslen)
class(strclslen)
class(lens)
lens <- strclslen %>% 
  as('Spatial') %>% 
  sp::SpatialLinesLengths(longlat = T)
?SpatialLinesLengths
strclslen <- strclslen %>% mutate(lens = lens) ##  length in KM?

save(strclslen, file = 'output_data/03_strclslen.RData', compress = 'xz')


# Stream class lengths for model sites ------------------------------------

# get stream class lengths for model sites
strclslen

thrsh <- 0.79; tails <- 0.1

load(file = 'output_data/02_spat.RData')

spat <- spat %>%
  rename(COMID = comid)
  
load(file = 'output_data/02_csci_comid.RData')

scrs <- csci_comid %>%
  dplyr::select(comid, stationcode, csci, latitude, longitude, sampledate) %>%
  rename(COMID = comid, StationCode = stationcode, lat = latitude, long = longitude, SampleDate = sampledate)

# names(scrs)
# names(csci_comid)
# data(spat)
# data(scrs)

dim(spat)
dim(scrs)
head(spat)

unique(spat$COMID) %in% unique(scrs$COMID)

# all cali hydro, really simplified
calinhd <- readOGR('/Users/katieirving/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHD_reaches_RB9_castreamclassification.shp') %>% 
  spTransform(prj) %>% 
  st_as_sf %>% 
  st_simplify(dTolerance = 0.5, preserveTopology = T) %>% 
  filter(COMID %in% unique(comid_prd$COMID)) %>% 
  dplyr::select(COMID, CLASS) %>%
  inner_join(comid_prd, by = 'COMID')

# ## define geom
coords <- scrs %>%
  dplyr::select(COMID, lat, long) %>%
  data.frame
class(coords)
coords

## add geometry to spat

spat <- left_join(spat,coords, by = "COMID")
class(spat)
## make spatial
scrs <- scrs %>%
  st_as_sf(coords=c("long", "lat"), crs=4326, remove=F)

spat <- spat %>%
  st_as_sf(coords=c("long", "lat"), crs=4326, remove=F)

head(scrs)
getcls2
# # get biological condition expectations
cls <- getcls2(spat, thrsh = thrsh, tails = tails, modls = 'core') #%>% 
  # mutate(COMID = as.character(COMID)) 

calicls <- calinhd %>% 
  left_join(cls, by = 'COMID')

calicls
length(na.omit(calicls$strcls)) ## 42

lens <- calicls %>% 
  dplyr::select(COMID, strcls) %>% 
  st_intersection(calipsa) %>%
  dplyr::select(COMID, strcls) %>% 
  drop_na(strcls) %>%
  as('Spatial') %>% 
  sp::SpatialLinesLengths(longlat = T)


sgrclslen <- cls %>% 
  mutate(lens = lens) #%>% ## not working here as different number of sites
group_by(strcls) %>% 
  summarise(lens = sum(lens)) %>% 
  filter(!is.na(strcls)) %>% 
  mutate(
    lenper = 100 *  lens / sum(lens), 
    lenper = round(lenper, 0)
  ) 

save(sgrclslen, file = 'data/sgrclslen.RData', compress = 'xz')




# Random forest for constraints -------------------------------------------


# rf model importance for constraints in each region

load(file = 'output_data/03_caliclsplo.RData')
load(file = 'output_data/03_strclslen.RData')

## upload flow

## hydro for prediction
delta <- read.csv("ignore/01_delta_h_long.csv")
head(delta)
dim(delta)

## format delta h. median per comid and metric, take only current conditions, make wide
delta <- delta %>%
  dplyr::select(-X, - FlowMetric) %>%
  filter(Scenario == "Current") %>%
  group_by(comid, hydro.endpoints) %>%
  summarise(MedDelta = median(DeltaH)) %>%
  rename(COMID = comid) %>%
  pivot_wider(names_from = hydro.endpoints, values_from = MedDelta)


# PSA by all comid
psaall <- strclslen %>% 
  filter(!is.na(strcls)) %>% 
  dplyr::select(COMID, PSA6)

psaall

st_geometry(psaall) <- NULL

allenv <- caliclsplo %>% 
  dplyr::select(COMID, strcls) %>% 
  unique %>% 
  left_join(delta, by = 'COMID') %>% 
  left_join(psaall, by = 'COMID') %>% 
  filter(!is.na(PSA6)) %>% 
  mutate(strcls = gsub('^possibly\\s|^likely\\s', '', strcls))


# remove wshed vars, focus on watershed only, remove COMID from model
cnstrfrst <- allenv[, !grepl('Cat|^COMID$', names(allenv))] %>% 
  group_by(PSA6) %>% 
  nest %>% 
  mutate(mods = purrr::map(data, function(x){
    
    tmp <- randomForest(as.factor(strcls) ~ .,
                        data = x, 
                        importance = TRUE, 
                        ntree = 1000, na.action = na.omit)
    
    tmp$importance
    
  })) %>% 
  dplyr::select(-data)

save(cnstrfrst, file = 'output_data/03_cnstrfrst.RData', compress = 'xz')
