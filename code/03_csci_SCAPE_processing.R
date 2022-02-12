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

dim(comid_prd)  ## 2333 sites

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

save(nhdplo, file = 'output_data/03_nhdplo.RData', compress = 'xz')


# Format stream class -----------------------------------------------------

# cali nhd stream classes, simplified, fortified, and saved

load(file = 'output_data/02_comid_prd.RData')

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

caliclsplo <- calicls %>% 
  as('Spatial') %>% 
  fortify %>% 
  left_join(caliclsid, by = 'id') 

save(calicls, file = 'output_data/03_calicls.RData', compress = 'xz') ## predicted percentiles wioth constrained class and stream class
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

######
# get stream class lengths in SGR
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

## define geom
coords <- scrs %>%
  select(COMID, lat, long) %>%
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
# get biological condition expectations
cls <- getcls2(spat, thrsh = thrsh, tails = tails, modls = 'core') #%>% 
  # mutate(COMID = as.character(COMID)) 
str(calinhd)
str(spat)
str(cls)
modcls <- spat %>% 
  left_join(cls, by = 'COMID')

clslen <- modcls %>% 
  dplyr::select(COMID, strcls) %>% 
  st_intersection(calipsa)
clslen
class(cls)
head(cls)
?SpatialLinesLengths
# spat <- as.data.frame(spat)

spat <- spat %>% 
  st_as_sf(coords=c("long", "lat"), crs=4326, remove=F)

# get percentage of stream lengths in each class
lens <- cls %>% 
  mutate(COMID = as.numeric(COMID)) %>% 
  left_join(spat, ., by = 'COMID') %>% 
  dplyr::select(COMID, strcls) %>% 
  as('Spatial') %>% 
  sp::SpatialLinesLengths(longlat = T)


class(lens)
sgrclslen <- cls %>% 
  mutate(lens = lens) %>% 
group_by(strcls) %>% 
  summarise(lens = sum(lens)) %>% 
  filter(!is.na(strcls)) %>% 
  mutate(
    lenper = 100 *  lens / sum(lens), 
    lenper = round(lenper, 0)
  ) 

save(sgrclslen, file = 'data/sgrclslen.RData', compress = 'xz')

######
# expected range of scores for urban, ag, other by region
data(calicls)
data(strclslen)

# streamcat data
strmcat <- rbind(read.csv("Z:/MarcusBeck/Landscape models from rafi/Streamcat_v2_AllCOMID_030117/exp_1.csv", stringsAsFactors = F),
                 read.csv("Z:/MarcusBeck/Landscape models from rafi/Streamcat_v2_AllCOMID_030117/exp_2.csv", stringsAsFactors = F),
                 read.csv("Z:/MarcusBeck/Landscape models from rafi/Streamcat_v2_AllCOMID_030117/exp_3.csv", stringsAsFactors = F))
comid.nats <- read.csv("Z:/MarcusBeck/Landscape models from rafi/ALL_COMID_Nats.csv", stringsAsFactors = F)
strmcat <- plyr::join(strmcat, comid.nats[,setdiff(names(comid.nats), "WsAreaSqKm")])

strmcat$TotUrb2011Ws<-  rowSums(strmcat[,c("PctUrbOp2011Ws","PctUrbLo2011Ws","PctUrbMd2011Ws","PctUrbHi2011Ws")])
strmcat$TotUrb2011Cat<-  rowSums(strmcat[,c("PctUrbOp2011Cat","PctUrbLo2011Cat","PctUrbMd2011Cat","PctUrbHi2011Cat")])
strmcat$TotUrb2011WsRp100<-  rowSums(strmcat[,c("PctUrbOp2011WsRp100","PctUrbLo2011WsRp100","PctUrbMd2011WsRp100","PctUrbHi2011WsRp100")])
strmcat$TotUrb2011CatRp100<-  rowSums(strmcat[,c("PctUrbOp2011CatRp100","PctUrbLo2011CatRp100","PctUrbMd2011CatRp100","PctUrbHi2011CatRp100")])

strmcat$TotAg2011Ws<-  rowSums(strmcat[,c("PctHay2011Ws","PctCrop2011Ws")])
strmcat$TotAg2011Cat<-  rowSums(strmcat[,c("PctHay2011Cat","PctCrop2011Cat")])
strmcat$TotAg2011WsRp100<-  rowSums(strmcat[,c("PctHay2011WsRp100","PctCrop2011WsRp100")])
strmcat$TotAg2011CatRp100<-  rowSums(strmcat[,c("PctHay2011CatRp100","PctCrop2011CatRp100")])

strmcat <- strmcat %>%
  dplyr::select(COMID, TotUrb2011Ws, TotAg2011Ws)

# PSA by all comid
psaall <- strclslen %>%
  dplyr::select(COMID, PSA6)
st_geometry(psaall) <- NULL

# data to summarize
scrdist <- calicls
st_geometry(scrdist) <- NULL
scrdist <- scrdist %>%
  dplyr::select(-strcls, -strcls_int) %>%
  left_join(strmcat, by = 'COMID') %>%
  left_join(psaall, by = 'COMID') %>%
  filter(!is.na(PSA6)) %>%
  rename(Region = PSA6)

# get range of scores for average urban, ag, open locations, by region
scrdistreg <- scrdist %>%
  group_by(Region) %>%
  nest %>%
  mutate(grps = purrr::map(data, function(x){
    
    # get kmeans, centers
    tomod <- x %>%
      mutate(
        Urb = log10(1 + TotUrb2011Ws),
        Ag = log10(1 + TotAg2011Ws)
      ) %>%
      dplyr::select(Urb, Ag)
    ngrps <- 8
    kmod <- kmeans(tomod, centers = ngrps, nstart = 10, iter.max = 100, algorithm = 'MacQueen')
    grps <- kmod$cluster
    cent <- kmod$centers %>%
      data.frame
    
    # find the typical ag, urb group
    urb <- which.max(cent[, 'Urb'])
    ag <- which.max(cent[, 'Ag'])
    oth <- which.min(rowSums(cent))
    
    grplabs <- list(
      urb = urb,
      ag = ag,
      oth = oth
    )
    
    ests <- x %>%
      mutate(
        grps = grps
      ) %>%
      filter(grps %in% unlist(grplabs)) %>%
      mutate(
        grps = factor(grps, levels = unlist(grplabs), labels = names(grplabs))
      ) %>%
      group_by(grps) %>%
      summarise(
        lo10 = mean(core0.10, na.rm = T),
        hi90 = mean(core0.90, na.rm = T),
        lo25 = mean(core0.25, na.rm = T),
        hi75 = mean(core0.75, na.rm = T),
        lo40 = mean(core0.40, na.rm = T),
        hi60 = mean(core0.60, na.rm = T)
      )
    
    return(ests)
    
  })) %>%
  dplyr::select(-data) %>%
  unnest %>%
  mutate(
    Region = factor(Region,
                    levels = c('Central Valley', 'Chaparral', 'Deserts Modoc', 'North Coast', 'Sierra Nevada', 'South Coast'),
                    labels = c('CV', 'CH', 'DM', 'NC', 'SN', 'SC')),
    Region = as.character(Region),
    grps = as.character(grps)
  )

# typical statewide
scrdistall <- scrdist %>%
  mutate(Region = 'Statewide') %>%
  group_by(Region) %>%
  nest %>%
  mutate(grps = purrr::map(data, function(x){
    
    # get kmeans, centers
    tomod <- x %>%
      mutate(
        Urb = log10(1 + TotUrb2011Ws),
        Ag = log10(1 + TotAg2011Ws)
      ) %>%
      dplyr::select(Urb, Ag)
    ngrps <- 8
    kmod <- kmeans(tomod, centers = ngrps, nstart = 10, iter.max = 100, algorithm="MacQueen")
    grps <- kmod$cluster
    cent <- kmod$centers %>%
      data.frame
    
    # find the typical ag, urb group
    urb <- which.max(cent[, 'Urb'])
    ag <- which.max(cent[, 'Ag'])
    oth <- which.min(rowSums(cent))
    
    grplabs <- list(
      urb = urb,
      ag = ag,
      oth = oth
    )
    
    ests <- x %>%
      mutate(
        grps = grps
      ) %>%
      filter(grps %in% unlist(grplabs)) %>%
      mutate(
        grps = factor(grps, levels = unlist(grplabs), labels = names(grplabs))
      ) %>%
      group_by(grps) %>%
      summarise(
        lo10 = mean(core0.10, na.rm = T),
        hi90 = mean(core0.90, na.rm = T),
        lo25 = mean(core0.25, na.rm = T),
        hi75 = mean(core0.75, na.rm = T),
        lo40 = mean(core0.40, na.rm = T),
        hi60 = mean(core0.60, na.rm = T)
      )
    
    return(ests)
    
  })) %>%
  dplyr::select(-data) %>%
  unnest %>%
  mutate(
    Region = as.character(Region),
    grps = as.character(grps)
  )


typscrs <- bind_rows(scrdistall, scrdistreg)

save(typscrs, file = 'data/typscrs.RData', compress = 'xz')

##
# rf model importance for constraints in each region

load(file = 'data/caliclsplo.RData')
load(file = 'data/strclslen.RData')

# streamcat data
strmcat <- rbind(read.csv("Z:/MarcusBeck/Landscape models from rafi/Streamcat_v2_AllCOMID_030117/exp_1.csv", stringsAsFactors = F),
                 read.csv("Z:/MarcusBeck/Landscape models from rafi/Streamcat_v2_AllCOMID_030117/exp_2.csv", stringsAsFactors = F),
                 read.csv("Z:/MarcusBeck/Landscape models from rafi/Streamcat_v2_AllCOMID_030117/exp_3.csv", stringsAsFactors = F))
comid.nats <- read.csv("Z:/MarcusBeck/Landscape models from rafi/ALL_COMID_Nats.csv", stringsAsFactors = F)
strmcat <- plyr::join(strmcat, comid.nats[,setdiff(names(comid.nats), "WsAreaSqKm")])

# PSA by all comid
psaall <- strclslen %>% 
  filter(!is.na(strcls)) %>% 
  dplyr::select(COMID, PSA6)
st_geometry(psaall) <- NULL

allenv <- caliclsplo %>% 
  dplyr::select(COMID, strcls) %>% 
  unique %>% 
  left_join(strmcat, by = 'COMID') %>% 
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

save(cnstrfrst, file = 'data/cnstrfrst.RData', compress = 'xz')
