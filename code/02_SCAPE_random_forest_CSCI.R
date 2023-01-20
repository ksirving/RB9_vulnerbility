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

outdir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/RB9_vulnerbility/figures"

# Upload all data ---------------------------------------------------------

# hydro for input

## flow data to build model
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

# all cali hydro, really simplified
streamClass <- readOGR('/Volumes/SpatialData/Spatial_Data/CA hydrologic Stream Class-harmonized/Final_Classification_9CLASS/Final_Classification_9CLASS.shp') %>% 
  spTransform(prj) %>% 
  st_as_sf %>% 
  st_simplify(dTolerance = 0.5, preserveTopology = T) 

# unique(streamClass$GNIS_NAME)

streamClass <- streamClass %>%
  select(CLASS, COMID) %>%
  rename(comid = COMID)

streamClass <- as.data.frame(streamClass)

unique(streamClass$CLASS)
unique(streamClass$comid)
## RB9 NHD reach

nhdReach <- readOGR('/Users/katieirving/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
nhdReach
# plot(nhdReach)
# plot(sites, add=T)

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
dim(csci)
### add stream class
# ?left_join
csci <- left_join(csci, streamClass, by = "comid") ## need stream class for NHD reaches of all bio sites
sum(is.na(csci$CLASS))
# 
# sum(nhdReach$COMID %in% csci$comid) ## 60

### stream class doesn't match, for now assign it randomly
dim(csci)
# 
# 420/3
# sc <- rep(c(3,8,7), each=140)
# length(sc)
# sc<- sample(sc)
# 
# csci <- csci %>%mutate(CLASS = sc)

# csci[which(csci$COMID==8264738),c("StationCode","COMID","AREA_SQKM","Largest")]
csci.full<-csci
csci<-csci.full[csci.full$Largest,]

## test stream class match

sum(streamClass$comid %in% csci.full$comid) ## 305

sum(csci.full$comid %in% streamClass$comid) ## 387

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


# Quantile Random Forest --------------------------------------------------------

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

rf_test.dat <- csci %>%
  # select(-FA_Mag) %>%
  filter(SiteSet=="Val")

sum(is.na(rf_full.dat))

## remove NAs
rf_full.dat <- na.omit(rf_full.dat)
rf_test.dat <- na.omit(rf_test.dat)


## model
set.seed(10101)
rf_full<-quantregForest(y=rf_full.dat$csci,
                        x=as.matrix(rf_full.dat[,c(full.vars)]),
                        keep.inbag=T, importance=T,proximity=T)

cor(rf_full$y, rf_full$predicted) ## 0.4526347
rf_full$predicted
rf_full$y
head(rf_full.dat)

rf.full.dat.res <- rf_full.dat %>%
  select(stationcode, comid, CLASS) %>%
  mutate(Obs = rf_full$y, Pred =  rf_full$predicted)


rf.full.dat.res


# Observed Vs Predicted ---------------------------------------------------

library(grid)
# pred1=predict(rf_full,type = "prob")

### quantiles at cal model sites
pred_at_mod_sites

pred_at_mod_sites <- predict(rf_full, what=seq(from=0.05, to=.95, by=.05), na.rm=T) %>%
  as.data.frame %>%
  mutate(comid = rf_full.dat$comid, Obs = rf_full.dat$csci, Pred =  rf_full$predicted, CLASS = rf_full.dat$CLASS)

## change names
names(pred_at_mod_sites) <- c(paste0("core",formatC(as.numeric(seq(from=0.05, to=.95, by=.05)), format = 'f', flag='0', digits = 2)), 'comid', "Obs", "Pred", "CLASS")

## predict on testing data
pred_at_test <- predict(rf_full, newdata = rf_test.dat[, full.vars], what=seq(from=0.05, to=.95, by=.05), na.rm=T) %>%
  as.data.frame %>%
  mutate(comid = rf_test.dat$comid, Obs = rf_test.dat$csci, CLASS = rf_test.dat$CLASS)

## change names
names(pred_at_test) <- c(paste0("core",formatC(as.numeric(seq(from=0.05, to=.95, by=.05)), format = 'f', flag='0', digits = 2)), 'comid', "Obs", "CLASS")

# cor(rf_test.dat$csci, pred_at_test$`quantile= 0.5`) ## 0.5198202

## training sites: pred from model

## get coefs

Correlation <- round(cor(pred_at_mod_sites$Obs, pred_at_mod_sites$Pred), digits = 2)
Rsq <- round(mean(rf_full$rsq), digits=2)

# Create a text
coefs <- grobTree(textGrob(paste0("Cor: ", Correlation, ", Rsq: ", Rsq),x=0.6,  y=0.05, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))

tr1 <- ggplot(pred_at_mod_sites, aes(x=Pred, y = Obs, color = CLASS)) +
  geom_point()+
  scale_x_continuous() +
  scale_y_continuous() +
  geom_abline(intercept = 0, slope = 1) +
  annotation_custom(coefs)

tr1

file.name1 <- paste0(outdir, "obs_v_pred_rf_at_reg_curve_sites.jpg")
ggsave(tr1, filename=file.name1, dpi=300, height=5, width=6)

## traing sites quantile 0.5 (median)

## get coefs

Correlation <- round(cor(pred_at_mod_sites$Obs, pred_at_mod_sites$core0.50), digits = 2)
Rsq <- round(mean(rf_full$rsq), digits=2)

# Create a text
coefs <- grobTree(textGrob(paste0("Cor: ", Correlation, ", Rsq: ", Rsq),x=0.6,  y=0.05, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))

tr2 <- ggplot(pred_at_mod_sites, aes(x=core0.50, y = Obs, color = CLASS)) +
  geom_point()+
  scale_x_continuous() +
  scale_y_continuous() +
  geom_abline(intercept = 0, slope = 1) +
  annotation_custom(coefs)

tr2

file.name1 <- paste0(outdir, "obs_v_pred_rf_at_reg_curve_sites_median_quantile.jpg")
ggsave(tr2, filename=file.name1, dpi=300, height=5, width=6)

## get coefs

Correlation <- round(cor(pred_at_mod_sites$Obs, pred_at_mod_sites$core0.75), digits = 2)
Rsq <- round(mean(rf_full$rsq), digits=2)

# Create a text
coefs <- grobTree(textGrob(paste0("Cor: ", Correlation, ", Rsq: ", Rsq),x=0.6,  y=0.05, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))

tr3 <- ggplot(pred_at_mod_sites, aes(x=core0.75, y = Obs, color = CLASS)) +
  geom_point()+
  scale_x_continuous() +
  scale_y_continuous() +
  geom_abline(intercept = 0, slope = 1) +
  annotation_custom(coefs)

tr3

file.name1 <- paste0(outdir, "obs_v_pred_rf_at_reg_curve_sites_0.75_quantile.jpg")
ggsave(tr3, filename=file.name1, dpi=300, height=5, width=6)

## testing sites: median quantile

## get coefs

Correlation <- round(cor(pred_at_test$Obs, pred_at_test$core0.50), digits = 2)
Rsq <- round(mean(rf_full$rsq), digits=2)

# Create a text
coefs <- grobTree(textGrob(paste0("Cor: ", Correlation, ", Rsq: ", Rsq),x=0.6,  y=0.05, hjust=0,
                           gp=gpar(col="red", fontsize=13, fontface="italic")))


head(pred_at_test)

te1 <- ggplot(pred_at_test, aes(x=core0.50, y = Obs, color = CLASS)) +
  geom_point()+
  scale_x_continuous() +
  scale_y_continuous() +
  geom_abline(intercept = 0, slope = 1)  +
  annotation_custom(coefs)

te1

file.name1 <- paste0(outdir, "obs_v_pred_rf_at_test_sites.jpg")
ggsave(te1, filename=file.name1, dpi=300, height=5, width=6)

# cor(pred_at_mod_sites$`quantile= 0.5`, rf_full$y) ## 0.9226458
# cor(pred_at_mod_sites$`quantile= 0.75`, rf_full$y) ## 0.8528288
# cor(pred_at_mod_sites$`quantile= 0.9`, rf_full$y) ## 0.6550466
# cor(pred_at_mod_sites$`quantile= 0.1`, rf_full$y) ## 0.6857495
# cor(pred_at_mod_sites$`quantile= 0.95`, rf_full$y) ## 0.5595889
# cor(pred_at_mod_sites$`quantile= 0.05`, rf_full$y) ## 0.5758296


spat <- pred_at_mod_sites ## needs geometry
pred_at_mod_sites 


# Extrapolate to RB9 NHDs -------------------------------------------------

# get model predictions, have to separate calibration oob from statewide

## format delta h. median per comid and metric, take only current conditions, make wide
delta <- delta %>%
  select(-X, - FlowMetric) %>%
  filter(Scenario == "Current") %>%
  group_by(comid, hydro.endpoints) %>%
  summarise(MedDelta = median(DeltaH)) %>%
  pivot_wider(names_from = hydro.endpoints, values_from = MedDelta)

delta

# prediction data w/o calibration dataset
newdatcr <- delta %>%
  filter(!comid %in% rf_full.dat$comid) %>%
  # select_(.dots = c('comid', core.candidates)) %>%
  na.omit

head(newdatcr)

# out of bag predictions for calibration dataset
# estimates for same comid must be averaged with oob estimates
predcore_oob <- predict(rf_full, what=seq(from=0.05, to=.95, by=.05), na.rm=T) %>%
  as.data.frame %>%
  mutate(comid = rf_full.dat$comid) %>%
  gather('var', 'val', -comid) %>%
  group_by(comid, var) %>%
  summarize(val = mean(val)) %>%
  spread(var, val) %>%
  .[, c(2:20, 1)]

dim(predcore_oob)## 221 n calibration sites
head(predcore_oob)

## predict on nhd data
predcore_all <- predict(rf_full, newdata = newdatcr[, -1], what=seq(from=0.05, to=.95, by=.05), na.rm=T) %>%
  as.data.frame %>%
  mutate(comid = newdatcr$comid)
## , wayr = newdatcr$wayr
dim(predcore_all) ## 68442
str(predcore_all)
length(unique(comid_prd$comid)) ## 2295

# join calibration oob with statewide
predcore <- bind_rows(predcore_oob, predcore_all)
head(predcore)
dim(predcore) ## 68663
names(predcore) <- c(paste0("core",formatC(as.numeric(seq(from=0.05, to=.95, by=.05)), format = 'f', flag='0', digits = 2)), 'comid')

pred_all <- predcore

pred_all$DevData<-
  ifelse(pred_all$comid %in% csci$comid[which(csci$SiteSet=="Cal")],"Cal",
         ifelse(pred_all$comid %in% csci$comid[which(csci$SiteSet=="Val")],"Val","No"))

head(pred_all)

comid_prd <- pred_all
comid_prd ## 68,663

# csci data for comparison with stream comid
csci_comid <- csci

# save all
save(spat, file = 'output_data/02_spat.RData', compress = 'xz')
save(csci_comid, file = 'output_data/02_csci_comid.RData', compress = 'xz')
save(comid_prd, file = 'output_data/02_comid_prd.RData', compress = 'xz')
save(rf_full,file = "models/02_rf_core.Rdata", compress = 'xz')
# save(rf_full,file = "C:/proj/manuscripts/landscape_mod/data/rf_full.Rdata", compress = 'xz')
