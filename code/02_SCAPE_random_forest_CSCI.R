### CSCI quantile random forest with flow

library(plyr)
library(dplyr)
library(reshape2)
library(sf)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(quantregForest)
library(tidyverse)
library(tidyr)
library(tidylog)


# Upload all data ---------------------------------------------------------

# hydro for input

## flow data
dh_data <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH_FINAL/SoCal_bio_deltaH_summary_supp_final.csv")

## subset median
dh_median <- subset(dh_data, summary.statistic =="median")
dim(dh_median) # 7236


## hydro for prediction
delta <- read.csv("ignore/01_delta_h_long.csv")
head(delta)
dim(delta)

## get RB9 metrics to subset from curve metrics
RB9_metrics <- unique(delta$hydro.endpoints)
RB9_metrics

## define projection
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

## stream class

streamClass <- readOGR('/Users/katieirving/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHD_reaches_RB9_castreamclassification.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
streamClass

# unique(streamClass$GNIS_NAME)

streamClass <- streamClass %>%
  select(CLASS, COMID) %>%
  rename(comid = COMID)

streamClass <- as.data.frame(streamClass)



## RB9 NHD reach

nhdReach <- readOGR('/Users/katieirving/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)

## csci data

csci <- readRDS("output_data/00_csci_comids.rds")
csci

# this finds out which station is most downstream if more than one on a COMID
csci$Largest<-
  sapply(1:nrow(csci), function(i){
    comid<-csci$comid[i]
    area<-csci$WatershedArea_Mi2[i]
    mydf<-csci[which(csci$comid==comid),]
    area==max(mydf$WatershedArea_Mi2)
  })

csci <- as.data.frame(csci)

### add stream class
# ?left_join
# csci <- left_join(csci, streamClass, by = "comid")
# sum(is.na(csci$CLASS))

### stream class doesn't match, for now assign it randomly
dim(csci)

420/8
sc <- rep(1:8, each=53)
length(sc)
sc<- sample(sc[-c(1:4)])

csci <- csci %>%mutate(CLASS = sc)

# csci[which(csci$COMID==8264738),c("StationCode","COMID","AREA_SQKM","Largest")]
csci.full<-csci
csci<-csci.full[csci.full$Largest,]

## test stream class match

sum(streamClass$comid %in% csci.full$comid) ## only 60 

sum(csci.full$comid %in% streamClass$comid) ## only 68

# Format bio data --------------------------------------------------

head(csci)


#Create cal and val data
sites<-unique(csci[,c("stationcode","CLASS", "Imperv")]) #Create a sites DF
sites


sites.d<-ddply(sites, .(CLASS), summarize, ImpT1=quantile(Imperv, probs=c(0.25)),ImpT2=quantile(Imperv, probs=c(0.5)),ImpT3=quantile(Imperv, probs=c(0.75)))
sites.d
# write.table(sites.d, "clipboard", sep="\t")

sites$RegImpQ<-   #Divide regions into thirds based on imperviousness
  sapply(1:nrow(sites), function(i){
    region<-sites$CLASS[i]
    imp<-sites$Imperv[i]
    imp.t1<-sites.d$ImpT1[which(sites.d$CLASS==region)]
    imp.t2<-sites.d$ImpT2[which(sites.d$CLASS==region)]
    imp.t3<-sites.d$ImpT3[which(sites.d$CLASS==region)]
    ifelse(imp<imp.t1,"T1", ifelse(imp<imp.t2,"T2", ifelse(imp<imp.t3,"T3","T4")))
  })
sites
table(sites$CLASS, sites$RegImpQ)
sites$Stratum<-paste(sites$CLASS, sites$RegImpQ, sep="_") #Create strata by dividing each region into thirds
table(sites$Stratum)

#Random assignment into cal (80%) and val (20%) data sets by Stratum field

sites.t<- sites %>% group_by(Stratum)
set.seed(500)
sites.cal<-sample_frac(sites.t, 0.75, replace=F)
sites$SiteSet<-ifelse(sites$stationcode %in% sites.cal$stationcode, "Cal","Val")
csci<-join(csci, sites[,c("stationcode", "RegImpQ", "Stratum", "SiteSet")])
csci
sites
#
#Select a single sample for assessment
csci.t<- csci %>% group_by(stationcode)
set.seed(501)
samps.sel<-sample_n(csci.t, 1)
# csci$SelectedSample<-ifelse(csci$SampleID %in% samps.sel$SampleID, "Selected","NotSelected")



# format flow data --------------------------------------------------------

head(csci)

## filter to only relevant metrics and columns, change site name, make wide for rf
# full.vars <- dh_median %>%
#   select(site, flow_metric, deltah_final) %>%
#   filter(flow_metric %in% RB9_metrics) %>%
#   rename(stationcode = site) %>%
#   pivot_wider(names_from = flow_metric, values_from = deltah_final)

names(csci)
full.vars <- names(csci[c(9, 10, 12:14)])
full.vars
## select only calibration data from csci and remove FA_Mag as too many nas

rf_full.dat <- csci %>%
  select(-FA_Mag) %>%
  filter(SiteSet=="Cal")

sum(is.na(rf_full.dat))

rf_full.dat <- na.omit(rf_full.dat)


# ?quantregForest
set.seed(10101)
rf_full<-quantregForest(y=rf_full.dat$csci,
                        x=as.matrix(rf_full.dat[,c(full.vars)]),
                        keep.inbag=T, importance=T,proximity=T)


# set.seed(10012)
# rf_core<-quantregForest(y=csci.rf.dat$CSCI,
#                         x=as.matrix(csci.rf.dat[,core.candidates]),
#                         keep.inbag=T, importance=T,proximity=T)



######
# get model predictions, have to separate calibration oob from statewide

## format delta h. median per comid and metric, take only current conditions, make wide
delta <- delta %>%
  select(-X, - FlowMetric) %>%
  filter(Scenario == "Current") %>%
  group_by(comid, hydro.endpoints) %>%
  summarise(MedDelta = median(DeltaH)) %>%
  pivot_wider(names_from = hydro.endpoints, values_from = MedDelta)
##
# core model

# prediction data w/o calibration dataset
newdatcr <- delta %>%
  filter(!comid %in% rf_full.dat$comid) %>%
  # select_(.dots = c('comid', core.candidates)) %>%
  na.omit

# out of bag predictions for calibration dataset
# estimates for same comid must be averaged with oob estimates
predcore_oob <- predict(rf_full, what=seq(from=0.05, to=.95, by=.05), na.rm=T) %>%
  as.data.frame %>%
  mutate(COMID = rf_full.dat$comid) %>%
  gather('var', 'val', -COMID) %>%
  group_by(COMID, var) %>%
  summarize(val = mean(val)) %>%
  spread(var, val) %>%
  .[, c(2:20, 1)]


predcore_all <- predict(rf_full, newdata = newdatcr[, -1], what=seq(from=0.05, to=.95, by=.05), na.rm=T) %>%
  as.data.frame %>%
  mutate(comid = newdatcr$comid)

# join calibration oob with statewide
predcore <- bind_rows(predcore_oob, predcore_all)
names(predcore) <- c(paste0("core",formatC(as.numeric(seq(from=0.05, to=.95, by=.05)), format = 'f', flag='0', digits = 2)), 'COMID')

pred_all <- predcore

# pred_all <- predcore %>%
#   left_join(predfull, by = 'COMID') %>%
#   left_join(all.comid[,c("COMID", core.candidates, setdiff(full.vars,core.candidates))], by = 'COMID') %>%
#   as.data.frame

pred_all$DevData<-
  ifelse(pred_all$COMID %in% csci$comid[which(csci$SiteSet=="Cal")],"Cal",
         ifelse(pred_all$COMID %in% csci$comid[which(csci$SiteSet=="Val")],"Val","No"))

comid_prd <- pred_all
comid_prd
# csci data for comparison with stream comid
csci_comid <- csci

##
# save all

save(csci_comid, file = 'output_data/02_csci_comid.RData', compress = 'xz')
save(comid_prd, file = 'output_data/02_comid_prd.RData', compress = 'xz')
save(rf_full,file = "models/02_rf_core.Rdata", compress = 'xz')
# save(rf_full,file = "C:/proj/manuscripts/landscape_mod/data/rf_full.Rdata", compress = 'xz')
