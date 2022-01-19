## relative change in probability of achieving a good bio score

## packages

library(tidyverse)
library(tidyr)
getwd()
out.dir <- "figures/"

## full names for labels
labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(Hydro_endpoint = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels


## work flow
# get probability of current delta H points in curve, try 1 subbasin across years to test
# get probability of water cons delta h points in curve
# compare them somehow, absolute values or median?


### delta h limits

limits <- read.csv("output_data/00_ALL_delta_thresholds_scaled.csv")
limits

limits <- left_join(limits, labels, by = "Hydro_endpoint")


# SOC data (to change with RB9 data) --------------------------------------


delta <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/00_Final_FFM_DeltaH_AH/SOC_deltaH_supp_final_12012021.csv")
head(delta)

## get delta h at each subbasin

delta_long <- delta %>%
  select(site, region, year, flow_metric, deltah_cur_ref_final, deltaH_watercon_ref_final) %>%
  pivot_longer(deltah_cur_ref_final:deltaH_watercon_ref_final, names_to = "Scenario", values_to = "DeltaH")

head(delta_long)

# delta_med <- delta_long %>%
#   group_by(site, region, flow_metric, Scenario) %>%
#   summarise(DeltaHMed = median(na.omit(DeltaH)))

delta_med 



# ASCI Curve data --------------------------------------------------------------------


metrics <-c("Q99", "SP_Tim","DS_Dur_WS")

all_asci <- read.csv("input_data/01_h_asci_neg_pos_logR_metrics_figures_April2021.csv")
head(all_asci)

## scale probability
all_asci <- all_asci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep="")) %>%
  rename(Hydro_endpoint = hydro.endpoints)

all_asci <- left_join(all_asci, labels, by ="Hydro_endpoint")


## subset to only important metrics
all_asci_sub <- subset(all_asci, Hydro_endpoint %in% metrics)


# CSCI Curve data--------------------------------------------------------------------


all_csci <- read.csv("input_data/01_csci_neg_pos_logR_metrics_figures_April2021.csv")
head(all_csci)

## scale probability
all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep="")) %>%
  rename(Hydro_endpoint = hydro.endpoints)

all_csci <- left_join(all_csci, labels, by ="Hydro_endpoint")


## subset to only important metrics
all_csci_sub <- subset(all_csci, Hydro_endpoint %in% metrics)


unique(all_csci_sub$Hydro_endpoint)


# delta H probability -----------------------------------------------------

## estimate probability of good score with all subbasins and years

# ASCI Probability --------------------------------------------------------

biol.endpoints<-c("H_ASCI", "D_ASCI")#

## hydro
hydro.endpoints<- unique(all_asci$hydro.endpoints)
hydro.endpoints
## thresholds

thresholds <- c(0.75, 0.86, 0.94) ## hybrid and diatom are the same

## make grid with all models 
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

## reduce glms and summary df to only rows needed
## find index to remove
ind1 <- which(bio_h_summary$biol.endpoints == "H_ASCI" & bio_h_summary$thresholds == 0.86)

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0( hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "models/01a_ASCI_negative_GLM_all_delta_mets_April2021.RData")
neg.glm <- neg.glm[ind1]

load(file = "models/01a_ASCI_positive_GLM_all_delta_mets_April2021.RData")
pos.glm <- pos.glm[ind1]

head(delta_long) ## new data to predict on

## define metrics
metrics <- unique(bio_h_summary$hydro.endpoints)

metrics
i=1
## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]

  hydroxx <- delta_long %>%
    filter(flow_metric == met)
  
  unique(hydroxx$flow_metric)
  
  ## get models for pos and neg
  posMod <- pos.glm[i][[1]]
  negMod <- neg.glm[i][[1]]
  
  ## rename to match models, separate scenarios and delta positive and negative
  new_data_current_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "deltah_cur_ref_final",
           !hydro < 0)
  
  new_data_current_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "deltah_cur_ref_final",
           !hydro >= 0)
  
  new_data_water_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "deltaH_watercon_ref_final",
           !hydro < 0)
  
  new_data_water_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "deltaH_watercon_ref_final",
           !hydro >= 0)
    

  ## predict current conditions
  posModCurrent <- predict(posMod, new_data_current_pos, type = "response")
  negModCurrent <- predict(negMod, new_data_current_neg, type = "response")
  
  ## predict water cons conditions
  posModWater <- predict(posMod, new_data_water_pos, type = "response")
  negModWater <- predict(negMod, new_data_water_neg, type = "response")
  
  ## add to dfs and scale
  ## current
  new_data_current_pos <-  new_data_current_pos %>%
    mutate(PredictedProbability = posModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) 
  
  new_data_current_neg <-  new_data_current_neg %>%
    mutate(PredictedProbability = negModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) 
  
  ## water conservation
  new_data_water_pos <-  new_data_water_pos %>%
    mutate(PredictedProbability = posModWater) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) 
  
  new_data_water_neg <-  new_data_water_neg %>%
    mutate(PredictedProbability = negModWater) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) 
  
  ## combine all data
  WaterProbs <- bind_rows( new_data_water_pos, new_data_water_neg)
  CurrentProbs <-  bind_rows(new_data_current_pos, new_data_current_neg)
  
  AllProbs <- bind_rows(WaterProbs, CurrentProbs)
  AllProbs <- AllProbs %>%
    select(-PredictedProbability, -hydro) %>%
    pivot_wider(names_from = "Scenario", values_from = "PredictedProbabilityScaled") %>%
    mutate(RelChange = (deltaH_watercon_ref_final-deltah_cur_ref_final)/deltah_cur_ref_final)
  
  AllProbsMed <- AllProbs %>%
    group_by(site, flow_metric, region) %>%
    summarise(MedChange = median(RelChange))
  
  ## save
  save(AllProbs, file = paste0("output_data/01_asci_rel_change_in_prob_", met, ".RData"))
  save(AllProbsMed, file = paste0("output_data/01_asci_median_rel_change_in_prob_", met, ".RData"))

  
}

# CSCI Probability --------------------------------------------------------


## hydro
hydro.endpoints<- unique(all_csci$Hydro_endpoint)
hydro.endpoints
## thresholds

thresholds <- c(0.63, 0.79, 0.92) 
biol.endpoints<-c("CSCI","OoverE","MMI")

## make grid with all models 
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

## reduce glms and summary df to only rows needed
## find index to remove
ind1 <- which(bio_h_summary$biol.endpoints == "CSCI" & bio_h_summary$thresholds == 0.79)

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0( hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "models/01_CSCI_negative_GLM_all_delta_mets_April2021.RData")
neg.glm <- neg.glm[ind1]

load(file = "models/01_CSCI_positive_GLM_all_delta_mets_April2021.RData")
pos.glm <- pos.glm[ind1]

head(delta_long) ## new data to predict on

length(pos.glm)
## define metrics
metrics <- unique(bio_h_summary$hydro.endpoints)

metrics
i=1
## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- delta_long %>%
    filter(flow_metric == met)
  
  unique(hydroxx$flow_metric)
  
  ## get models for pos and neg
  posMod <- pos.glm[i][[1]]
  negMod <- neg.glm[i][[1]]
  
  ## rename to match models, separate scenarios and delta positive and negative
  new_data_current_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "deltah_cur_ref_final",
           !hydro < 0)
  
  new_data_current_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "deltah_cur_ref_final",
           !hydro >= 0)
  
  new_data_water_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "deltaH_watercon_ref_final",
           !hydro < 0)
  
  new_data_water_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "deltaH_watercon_ref_final",
           !hydro >= 0)
  
  
  ## predict current conditions
  posModCurrent <- predict(posMod, new_data_current_pos, type = "response")
  negModCurrent <- predict(negMod, new_data_current_neg, type = "response")
  
  ## predict water cons conditions
  posModWater <- predict(posMod, new_data_water_pos, type = "response")
  negModWater <- predict(negMod, new_data_water_neg, type = "response")
  
  ## add to dfs and scale
  ## current
  new_data_current_pos <-  new_data_current_pos %>%
    mutate(PredictedProbability = posModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) 
  
  new_data_current_neg <-  new_data_current_neg %>%
    mutate(PredictedProbability = negModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) 
  
  ## water conservation
  new_data_water_pos <-  new_data_water_pos %>%
    mutate(PredictedProbability = posModWater) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) 
  
  new_data_water_neg <-  new_data_water_neg %>%
    mutate(PredictedProbability = negModWater) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) 
  
  ## combine all data
  WaterProbs <- bind_rows( new_data_water_pos, new_data_water_neg)
  CurrentProbs <-  bind_rows(new_data_current_pos, new_data_current_neg)
  
  AllProbs <- bind_rows(WaterProbs, CurrentProbs)
  AllProbs <- AllProbs %>%
    select(-PredictedProbability, -hydro) %>%
    pivot_wider(names_from = "Scenario", values_from = "PredictedProbabilityScaled") %>%
    mutate(RelChange = (deltaH_watercon_ref_final-deltah_cur_ref_final)/deltah_cur_ref_final)
  
  AllProbsMed <- AllProbs %>%
    group_by(site, flow_metric, region) %>%
    summarise(MedChang = median(RelChange))
  
  
  ## save
  save(AllProbs, file = paste0("output_data/01_csci_rel_change_in_prob_", met, ".RData"))
  save(AllProbsMed, file = paste0("output_data/01_csci_median_rel_change_in_prob_", met, ".RData"))
  
  
}




# Boxplots of relative change ---------------------------------------------

# upload and combine change values

rel <- list.files(path = "output_data", pattern ="asci_rel_change")

AllProbx <- NULL

for(i in 1:length(rel)) {
  
  load(file = paste0("output_data/", rel[i]))
  
  AllProbx <- bind_rows(AllProbx, AllProbs)
  
}

AllProbx <- AllProbx %>%
  mutate(flow_metric = as.factor(flow_metric))

min(na.omit(AllProbx$RelChange))

p1 <- ggplot(AllProbx, aes(x=flow_metric, y = RelChange)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Probability", limits = c(-1,1)) 

p1

file.name1 <- paste0(out.dir, "ASCI_change_in_prob_boxplots.jpg")
ggsave(p1, filename=file.name1, dpi=300, height=5, width=6)

## csci

rel <- list.files(path = "output_data", pattern ="csci_rel_change")

AllProbx <- NULL

for(i in 1:length(rel)) {
  
  load(file = paste0("output_data/", rel[i]))
  
  AllProbx <- bind_rows(AllProbx, AllProbs)
  
}

AllProbx <- AllProbx %>%
  mutate(flow_metric = as.factor(flow_metric))

min(na.omit(AllProbx$RelChange))

p1 <- ggplot(AllProbx, aes(x=flow_metric, y = RelChange)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Probability", limits = c(-1,1)) 

p1

file.name1 <- paste0(out.dir, "CSCI_change_in_prob_boxplots.jpg")
ggsave(p1, filename=file.name1, dpi=300, height=5, width=6)


# Map change in probability -----------------------------------------------


