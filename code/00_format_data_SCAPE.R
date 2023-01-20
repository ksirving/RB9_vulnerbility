## formatting csci and asci data for SCAPE

library(reshape2)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(tidyverse)
library(sf)

soc <- read.csv("input_data/CSCI_ASCI_SitesAll_for_SOC_FlowEcologyStudy.csv")
head(soc)
dim(soc) ## 841
unique(soc$BugID)

## hydro for prediction
delta <- read.csv("ignore/01_delta_h_long.csv")
head(delta)
dim(delta)

## get RB9 metrics to subset from curve metrics
RB9_metrics <- unique(delta$hydro.endpoints)
RB9_metrics

## bug sites - add more data for RB9!!!!!

csci <- read.csv("input_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
dim(csci)
head(csci)

csci_sites <- unique(csci$stationcode)
soc_sites <- unique(soc$stationcode)

csci_sites
soc_sites

soc_sites %in% csci_sites
sum(soc_sites %in% csci_sites) ## 420

## keep only RB9 metrics - mag etc
csci <- csci %>%
  select(-X) %>%
  pivot_longer(DS_Dur_WS:Wet_Tim, names_to = "flow_metric", values_to = "deltah") %>%
  filter(flow_metric %in% RB9_metrics) %>%
  pivot_wider(names_from = "flow_metric", values_from = "deltah")

csci



# Get COMIDs --------------------------------------------------------------


## make spatial
library(nhdplusTools)

csci <- csci %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F)

# use nhdtools to get comids
csci_coms <- csci %>%
  group_split(stationcode) %>%
  set_names(., csci$stationcode) %>%
  map(~discover_nhdplus_id(.x$geometry))

csci_coms

# flatten into single dataframe instead of list
csci_coms_df <-csci_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("comid"=V1) %>% rownames_to_column(var = "stationcode")

csci_coms_df

### join back to CSCI DF
head(csci)

csci_all_coms <- full_join(csci, csci_coms_df, by = "stationcode")
csci_all_coms

## add to soc sites data

head(soc)

soc <- soc %>%
  select(BugID, WatershedArea_Mi2, Imperv) %>%
  rename(stationcode = BugID)

csci_all_coms_imperv <- full_join(csci_all_coms, soc, by = "stationcode")
dim(csci_all_coms_imperv)

csci_all_coms_imperv <- csci_all_coms_imperv %>% ## NAs are ones that don't match and not in curve data
    drop_na(comid)
## save

write_rds(csci_all_coms_imperv, file = "output_data/00_csci_comids.rds")



# ASCI --------------------------------------------------------------------


library(reshape2)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(tidyverse)
library(sf)

soc <- read.csv("input_data/CSCI_ASCI_SitesAll_for_SOC_FlowEcologyStudy.csv")
head(soc)
dim(soc) ## 841
unique(soc$BugID)

soc <- soc %>%
  rename(StationCode = BugID)

sites <- soc %>%
  select(StationCode, Lat, Long)

## hydro for prediction
delta <- read.csv("ignore/01_delta_h_long.csv")
head(delta)
dim(delta)

## get RB9 metrics to subset from curve metrics
RB9_metrics <- unique(delta$hydro.endpoints)
RB9_metrics

## bug sites - add more data for RB9!!!!!

asci <- read.csv("input_data/00_asci_delta_formatted_median_Nov2021.csv")
dim(asci)
head(asci)

asci <- asci %>%
  left_join(sites, by = "StationCode")

asci_sites <- unique(asci$StationCode)
soc_sites <- unique(soc$StationCode)

asci_sites
soc_sites

soc_sites %in% asci_sites
sum(soc_sites %in% asci_sites) ## 420

## keep only RB9 metrics - mag etc
asci <- asci %>%
  select(-X) %>%
  pivot_longer(DS_Dur_WS:Wet_Tim, names_to = "flow_metric", values_to = "deltah") %>%
  filter(flow_metric %in% RB9_metrics) %>%
  pivot_wider(names_from = "flow_metric", values_from = "deltah")

asci



# Get COMIDs --------------------------------------------------------------


## make spatial
library(nhdplusTools)

asci <- asci %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326, remove=F)

# use nhdtools to get comids
asci_coms <- asci %>%
  group_split(StationCode) %>%
  set_names(., asci$StationCode) %>%
  map(~discover_nhdplus_id(.x$geometry))

asci_coms

# flatten into single dataframe instead of list
asci_coms_df <-asci_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("comid"=V1) %>% rownames_to_column(var = "StationCode")

asci_coms_df

### join back to asci DF
head(asci)

asci_all_coms <- full_join(asci, asci_coms_df, by = "StationCode")
asci_all_coms

## add to soc sites data

head(soc)

soc <- soc %>%
  select(BugID, WatershedArea_Mi2, Imperv) %>%
  rename(stationcode = BugID)

asci_all_coms_imperv <- full_join(asci_all_coms, soc, by = "StationCode")
dim(asci_all_coms_imperv)

asci_all_coms_imperv <- asci_all_coms_imperv %>% ## NAs are ones that don't match and not in curve data
  drop_na(comid)
## save

write_rds(asci_all_coms_imperv, file = "output_data/00_asci_comids.rds")


# combine all comids ------------------------------------------------------

asci <-readRDS(file = "output_data/00_asci_comids.rds")
asci <- asci %>%
  select(StationCode, comid)

csci <-readRDS(file = "output_data/00_csci_comids.rds")
csci
csci <- csci %>%
  rename(StationCode = stationcode) %>%
  select(StationCode, comid)

all_data <- bind_rows(asci, csci)

head(all_data)
dim(all_data)

all_data <- all_data %>% distinct()

write.csv(all_data, "output_data/00_csci_asci_sites_comids.csv")
