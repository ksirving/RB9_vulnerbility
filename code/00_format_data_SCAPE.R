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



