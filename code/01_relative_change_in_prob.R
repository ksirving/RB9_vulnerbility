## relative change in probability of achieving a good bio score

## packages

library(tidyverse)
library(tidyr)
library(tidylog)

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

delta <- read.csv("input_data/2022-01-25_RFpred_output_alldata_SDCOMIDs.csv")
head(delta)
dim(delta)

## remove duplicates
delta <- delta %>% distinct()

## add dummy water year type
wyt_df <- data.frame(unique(delta$wayr))
colnames(wyt_df)[1] <- "wayr"
wyt_df$wytx[1:22] <- "dry"
wyt_df$wytx[23:44] <- "moderate"
wyt_df$wytx[45:66] <- "wet"

wyt_df$wyt <- sample(wyt_df$wytx)
wyt_df$wytx <- NULL

wyt_df

unique(delta$wayr)


# test <- delta %>%
#   filter(comid == "20350539")

## get delta h at each subbasin

delta_long <- delta %>%
  # select(comid region, year, flow_metric, deltah_cur_ref_final, deltaH_watercon_ref_final) %>%
  pivot_longer(d_ds_mag_50:d_wet_bfl_mag_50, names_to = "FlowMetric", values_to = "DeltaH")

head(delta_long)



## split the time series into historical and current

## historical = 1950-1982
## current = 1983 - 2015

## change metric names to match curve data
## remove peak metrics


unique(delta_long$FlowMetric)
unique(all_asci$Hydro_endpoint)

delta_long <- delta_long %>%
  mutate(Scenario = ifelse(wayr %in% 1950:1982, "Historical", "Current")) %>%
  mutate(hydro.endpoints = case_when(FlowMetric == "d_ds_mag_50" ~ "DS_Mag_50",
                                     FlowMetric == "d_ds_mag_90" ~ "DS_Mag_90",
                                     FlowMetric == "d_fa_mag" ~ "FA_Mag",
                                     # FlowMetric == "d_peak_10" ~ "DS_Mag_50",
                                     # FlowMetric == "d_peak_2" ~ "DS_Mag_50",
                                     # FlowMetric == "d_peak_5" ~ "DS_Mag_50",
                                     FlowMetric == "d_sp_mag" ~ "SP_Mag",
                                     FlowMetric == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     FlowMetric == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50",)) %>%
  filter(!FlowMetric %in% c("d_peak_10", "d_peak_2", "d_peak_5"))



## add year id
## define years to be assigned in scenrios
wayr1 <- seq(1950, 1982, 1)
wayr1

wayr2 <- seq(1983, 2015, 1)
wayr2

## define ID sequence
IDs <- seq(1, 33,1)
IDs

## join years in each scenario with IDs
yearDF1 <- data.frame(wayr1, IDs)
colnames(yearDF1)[1] <- "wayr"

yearDF2 <- data.frame(wayr2, IDs)
colnames(yearDF2)[1] <- "wayr"

## combine scenario years
yearDF <- bind_rows(yearDF1, yearDF2)
yearDF

## add back to DF
delta_long <- left_join(delta_long, yearDF, by = "wayr")

head(delta_long)
sum(is.na(delta_long))

RB9_metrics <- unique(delta_long$hydro.endpoints)
RB9_metrics


# test <- delta_long %>%
#   filter(comid == "20350539",hydro.endpoints == "DS_Mag_50" )



# ASCI Curve data --------------------------------------------------------------------


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
# all_asci_sub <- subset(all_asci, Hydro_endpoint %in% metrics)


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
# all_csci_sub <- subset(all_csci, Hydro_endpoint %in% metrics)

# delta H probability -----------------------------------------------------

## estimate probability of good score with all subbasins and years

# ASCI Probability --------------------------------------------------------

biol.endpoints<-c("H_ASCI", "D_ASCI")#
names(all_asci)
## hydro
hydro.endpoints<- unique(all_asci$Hydro_endpoint)
hydro.endpoints
## thresholds

thresholds <- c(0.75, 0.86, 0.94) ## hybrid and diatom are the same

## make grid with all models 
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

## reduce glms and summary df to only rows needed
## find index to remove
ind1 <- which(bio_h_summary$biol.endpoints == "H_ASCI" & bio_h_summary$thresholds == 0.86 
              & bio_h_summary$hydro.endpoints %in% RB9_metrics)

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0( hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "models/01a_ASCI_negative_GLM_all_delta_mets_April2021.RData")
neg.glm <- neg.glm[ind1]
neg.glm
load(file = "models/01a_ASCI_positive_GLM_all_delta_mets_April2021.RData")
pos.glm <- pos.glm[ind1]

head(delta_long) ## new data to predict on
dim(delta_long)
## define metrics
metrics <- unique(bio_h_summary$hydro.endpoints)

metrics
i=1
## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]

  hydroxx <- delta_long %>%
    filter(hydro.endpoints == met)
  
  unique(hydroxx$FlowMetric)
  
  ## get models for pos and neg
  posMod <- pos.glm[i][[1]]
  negMod <- neg.glm[i][[1]]
  
  ## rename to match models, separate scenarios and delta positive and negative
  new_data_current_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Current",
           !hydro < 0)
  
  new_data_current_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Current",
           !hydro >= 0)
  
  new_data_Hist_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Historical",
           !hydro < 0)
  
  new_data_Hist_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Historical",
           !hydro >= 0)
    

  ## predict current conditions
  posModCurrent <- predict(posMod, new_data_current_pos, type = "response")
  negModCurrent <- predict(negMod, new_data_current_neg, type = "response")
  
  ## predict water cons conditions
  posModHist <- predict(posMod, new_data_Hist_pos, type = "response")
  negModHist <- predict(negMod, new_data_Hist_neg, type = "response")
  
  ## add to dfs and scale
  ## current
  new_data_current_pos <-  new_data_current_pos %>%
    mutate(PredictedProbability = posModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
    # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
    #          (max(hydro)-min(hydro))) 
  
  new_data_current_pos
  
  new_data_current_neg <-  new_data_current_neg %>%
    mutate(PredictedProbability = negModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
    # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
    #          (max(hydro)-min(hydro)))  
  
  ## Hist conservation
  new_data_Hist_pos <-  new_data_Hist_pos %>%
    mutate(PredictedProbability = posModHist) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
    # mutate(HistDeltaNorm = (hydro-min(hydro))/
    #          (max(hydro)-min(hydro))) 
  
  new_data_Hist_neg <-  new_data_Hist_neg %>%
    mutate(PredictedProbability = negModHist) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) #%>%
    # mutate(HistDeltaNorm = (hydro-min(hydro))/
    #          (max(hydro)-min(hydro))) 
  
  ## combine all data
  HistProbs <- bind_rows(new_data_Hist_pos, new_data_Hist_neg)
  
  ## add water year type
  HistProbs <- right_join( HistProbs, wyt_df, by = "wayr")
  
  ## calculate median per site per wyt
  HistProbsMed <-   HistProbs %>%
    group_by(comid, hydro.endpoints, wyt) %>%
    summarise(MedProb = median(PredictedProbabilityScaled), 
              MedDelta = median(hydro)) %>%
    mutate(Scenario = "Historical")

  ## combine current
  CurrentProbs <-  bind_rows(new_data_current_pos, new_data_current_neg)
  ## add water year type
  CurrentProbs <- right_join(CurrentProbs, wyt_df, by = "wayr")
  
  ## calculate median per site per wyt
  CurrentProbsMed <-   CurrentProbs %>%
    group_by(comid, hydro.endpoints, wyt) %>%
    summarise(MedProb = median(PredictedProbabilityScaled), 
              MedDelta = median(hydro)) %>%
    mutate(Scenario = "Current")
  
  ## combine historical and current
  
  AllProbs <- bind_rows(HistProbsMed, CurrentProbsMed)
  names(AllProbs)
  
  AllDelta <- AllProbs %>%
    select(-MedProb) %>%
    # group_by(comid, IDs, hydro.endpoints)
    pivot_wider(names_from = "Scenario", values_from = "MedDelta") %>%
    mutate(RelChangeDelta = (Current-Historical)/Historical) %>%
    mutate(AbsChangeDelta = (Current-Historical)) 

  AllProbs <- AllProbs %>%
    select(-MedDelta) %>%
    # mutate(PredictedProbabilityScaled = ifelse(PredictedProbabilityScaled == 0, 0.00001, PredictedProbabilityScaled)) %>%
    pivot_wider(names_from = "Scenario", values_from = "MedProb") %>%
    mutate(RelChange = (Current-Historical)/Historical) %>%
    mutate(AbsChange = (Current-Historical)) 
  
  
  # AllProbsMed <- AllProbs %>%
  #   group_by(comid, IDs, hydro.endpoints) %>%
  #   summarise(MedChange = median(RelChange))
  
  ## save
  save(AllDelta, file = paste0("output_data/01_asci_rel_change_in_delta_", met, ".RData"))
  save(AllProbs, file = paste0("output_data/01_asci_rel_change_in_prob_", met, ".RData"))
  # save(AllProbsMed, file = paste0("output_data/01_asci_median_rel_change_in_prob_", met, ".RData"))

  
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
ind1 <- which(bio_h_summary$biol.endpoints == "CSCI" & bio_h_summary$thresholds == 0.79
              & bio_h_summary$hydro.endpoints %in% RB9_metrics)

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0( hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "models/01_CSCI_negative_GLM_all_delta_mets_April2021.RData")
neg.glm <- neg.glm[ind1]
neg.glm 
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
    filter(hydro.endpoints == met)
  
  unique(hydroxx$FlowMetric)
  
  ## get models for pos and neg
  posMod <- pos.glm[i][[1]]
  negMod <- neg.glm[i][[1]]
  
  ## rename to match models, separate scenarios and delta positive and negative
  new_data_current_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Current",
           !hydro < 0)
  
  new_data_current_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Current",
           !hydro >= 0)
  
  new_data_Hist_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Historical",
           !hydro < 0)
  
  new_data_Hist_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Historical",
           !hydro >= 0)
  
  
  ## predict current conditions
  posModCurrent <- predict(posMod, new_data_current_pos, type = "response")
  negModCurrent <- predict(negMod, new_data_current_neg, type = "response")
  
  ## predict water cons conditions
  posModHist <- predict(posMod, new_data_Hist_pos, type = "response")
  negModHist <- predict(negMod, new_data_Hist_neg, type = "response")
  
  ## add to dfs and scale
  ## current
  new_data_current_pos <-  new_data_current_pos %>%
    mutate(PredictedProbability = posModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
  # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
  #          (max(hydro)-min(hydro))) 
  
  new_data_current_pos
  
  new_data_current_neg <-  new_data_current_neg %>%
    mutate(PredictedProbability = negModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
  # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
  #          (max(hydro)-min(hydro)))  
  
  ## Hist conservation
  new_data_Hist_pos <-  new_data_Hist_pos %>%
    mutate(PredictedProbability = posModHist) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
  # mutate(HistDeltaNorm = (hydro-min(hydro))/
  #          (max(hydro)-min(hydro))) 
  
  new_data_Hist_neg <-  new_data_Hist_neg %>%
    mutate(PredictedProbability = negModHist) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability))) #%>%
  # mutate(HistDeltaNorm = (hydro-min(hydro))/
  #          (max(hydro)-min(hydro))) 
  
  ## combine all data
  HistProbs <- bind_rows(new_data_Hist_pos, new_data_Hist_neg)
  
  ## add water year type
  HistProbs <- right_join( HistProbs, wyt_df, by = "wayr")
  
  ## calculate median per site per wyt
  HistProbsMed <-   HistProbs %>%
    group_by(comid, hydro.endpoints, wyt) %>%
    summarise(MedProb = median(PredictedProbabilityScaled), 
              MedDelta = median(hydro)) %>%
    mutate(Scenario = "Historical")
  
  ## combine current
  CurrentProbs <-  bind_rows(new_data_current_pos, new_data_current_neg)
  ## add water year type
  CurrentProbs <- right_join(CurrentProbs, wyt_df, by = "wayr")
  
  ## calculate median per site per wyt
  CurrentProbsMed <-   CurrentProbs %>%
    group_by(comid, hydro.endpoints, wyt) %>%
    summarise(MedProb = median(PredictedProbabilityScaled), 
              MedDelta = median(hydro)) %>%
    mutate(Scenario = "Current")
  
  ## combine historical and current
  
  AllProbs <- bind_rows(HistProbsMed, CurrentProbsMed)
  names(AllProbs)
  
  AllDelta <- AllProbs %>%
    select(-MedProb) %>%
    # group_by(comid, IDs, hydro.endpoints)
    pivot_wider(names_from = "Scenario", values_from = "MedDelta") %>%
    mutate(RelChangeDelta = (Current-Historical)/Historical) %>%
    mutate(AbsChangeDelta = (Current-Historical)) 
  
  AllProbs <- AllProbs %>%
    select(-MedDelta) %>%
    # mutate(PredictedProbabilityScaled = ifelse(PredictedProbabilityScaled == 0, 0.00001, PredictedProbabilityScaled)) %>%
    pivot_wider(names_from = "Scenario", values_from = "MedProb") %>%
    mutate(RelChange = (Current-Historical)/Historical) %>%
    mutate(AbsChange = (Current-Historical)) 
  
  
  # AllProbsMed <- AllProbs %>%
  #   group_by(comid, IDs, hydro.endpoints) %>%
  #   summarise(MedChange = median(RelChange))
  
  ## save
  save(AllDelta, file = paste0("output_data/01_csci_rel_change_in_delta_", met, ".RData"))
  save(AllProbs, file = paste0("output_data/01_csci_rel_change_in_prob_", met, ".RData"))
  # save(AllProbsMed, file = paste0("output_data/01_csci_median_rel_change_in_prob_", met, ".RData"))
  
  
}



# Boxplots of relative change ---------------------------------------------

# upload and combine change values

rel <- list.files(path = "output_data", pattern ="asci_rel_change_in_prob")
rel
AllProbx <- NULL

for(i in 1:length(rel)) {
  
  load(file = paste0("output_data/", rel[i]))
  
  AllProbx <- bind_rows(AllProbx, AllProbs)
  
}

names(AllProbx)

asciProbs <- AllProbx


asciProbs <- asciProbs %>%
  mutate(flow_metric = as.factor(hydro.endpoints)) 

sum(is.na(asciProbs$hydro.endpoints))

asciProbs <- na.omit(asciProbs)

### assiugn water year type to compare like for like?

min(na.omit(asciProbs$RelChange))

p1a <- ggplot(asciProbs, aes(x=flow_metric, y = RelChange)) +
  geom_boxplot() +
  facet_wrap(~wyt) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Probability") 

p1a

file.name1 <- paste0(out.dir, "ASCI_change_in_prob_boxplots_relative.jpg")
ggsave(p1a, filename=file.name1, dpi=300, height=5, width=6)

p2a <- ggplot(asciProbs, aes(x=flow_metric, y = AbsChange)) +
  geom_boxplot() +
  facet_wrap(~wyt) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Probability") 

p2a

file.name1 <- paste0(out.dir, "ASCI_change_in_prob_boxplots_absolute.jpg")
ggsave(p2a, filename=file.name1, dpi=300, height=5, width=6)

## csci

rel <- list.files(path = "output_data", pattern ="csci_rel_change_in_prob")
rel
AllProbx <- NULL

for(i in 1:length(rel)) {
  
  load(file = paste0("output_data/", rel[i]))
  
  AllProbx <- bind_rows(AllProbx, AllProbs)
  
}

csciProbs <- AllProbx

csciProbs <-csciProbs %>%
  mutate(flow_metric = as.factor(hydro.endpoints)) 

sum(is.na(csciProbs$hydro.endpoints))

csciProbs <- na.omit(csciProbs)

min(na.omit(csciProbs$RelChange))

p1c <- ggplot(csciProbs, aes(x=flow_metric, y = RelChange)) +
  geom_boxplot() +
  facet_wrap(~wyt) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Probability") 

p1c

file.name1 <- paste0(out.dir, "CSCI_change_in_prob_boxplots_relative.jpg")
ggsave(p1c, filename=file.name1, dpi=300, height=5, width=6)

p2c <- ggplot(csciProbs, aes(x=flow_metric, y = AbsChange)) +
  geom_boxplot() +
  facet_wrap(~wyt) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Probability") 

p2c

file.name1 <- paste0(out.dir, "CSCI_change_in_prob_boxplots_absolute.jpg")
ggsave(p2c, filename=file.name1, dpi=300, height=5, width=6)


# Delta H figure -----------------------------------------------

## function to normailise between -1 and 1
ReScale <- function(x,first,last){(last-first)/(max(x)-min(x))*(x-min(x))+first}

# upload and combine change values - same for both ASCI and CSCI

rel <- list.files(path = "output_data", pattern ="asci_rel_change_in_delta")
rel
AllDeltax <- NULL

for(i in 1:length(rel)) {
  
  load(file = paste0("output_data/", rel[i]))
  
  AllDeltax <- bind_rows(AllDeltax, AllDelta)
  
}

names(AllDeltax)
AllDeltax <- na.omit(AllDeltax) ### fix inf values in loop!!!!!!!!!

Deltax <- AllDeltax %>%
  mutate(flow_metric = as.factor(hydro.endpoints)) %>%
  group_by(flow_metric, wyt) %>%
  mutate(AbsChangeDeltaNorm = (AbsChangeDelta+min(AbsChangeDelta))/
                           (max(AbsChangeDelta)-min(AbsChangeDelta))) %>%
  mutate(AbsChangeDeltaScale = ReScale(AbsChangeDelta, -1,1))


p1a <- ggplot(Deltax, aes(x=flow_metric, y = RelChangeDelta)) +
  geom_boxplot() +
  facet_wrap(~wyt) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Delta H") 
  # scale_y_log10(name="Change in Delta H") 

p1a

file.name1 <- paste0(out.dir, "Change_in_delta_boxplots_relative.jpg")
ggsave(p1a, filename=file.name1, dpi=300, height=5, width=6)

p2a <- ggplot(Deltax, aes(x=flow_metric, y = AbsChangeDelta)) +
  geom_boxplot() +
  facet_wrap(~wyt) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Delta H") 
  # scale_y_log10(name="Change in Delta H") 

p2a

file.name1 <- paste0(out.dir, "Change_in_delta_boxplots_absolute.jpg")
ggsave(p2a, filename=file.name1, dpi=300, height=5, width=6)

p3a <- ggplot(Deltax, aes(x=flow_metric, y = AbsChangeDeltaScale)) +
  geom_boxplot() +
  facet_wrap(~wyt) +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Delta H") 
# scale_y_log10(name="Change in Delta H") 

p3a

file.name1 <- paste0(out.dir, "Change_in_delta_boxplots_absolute_scale.jpg")
ggsave(p3a, filename=file.name1, dpi=300, height=5, width=6)


# Probability and Delta plot ----------------------------------------------

### combine data

head(asciProbs)
head(csciProbs)
head(Deltax)

asci <- asciProbs %>%
  ungroup() %>%
  select(comid, flow_metric, wyt, AbsChange) %>%
  rename(ASCI = AbsChange)

csci <- csciProbs %>%
  ungroup() %>%
  select(comid, flow_metric, wyt, AbsChange) %>%
  rename(CSCI = AbsChange)

deltaH <- Deltax %>%
  select(comid, flow_metric, wyt, AbsChangeDeltaScale) %>%
  rename(DeltaH = AbsChangeDeltaScale)

head(asci)
head(csci)
head(deltaH)

bio <- full_join(asci, csci, by =c("comid", "flow_metric", "wyt")) ## some didn't match, fix!!!
allData <- full_join(bio, deltaH, by =c("comid", "flow_metric", "wyt")) ## some didn't match, fix!!!

head(allData)

## make longer
allData <- allData %>%
  pivot_longer(ASCI:CSCI, names_to = "Bio", values_to = "Change")


## plot
ggplot(allData, aes(x=DeltaH, y = Change)) +
  geom_point(aes(col = wyt)) +
  facet_grid(rows = vars(Bio), cols = vars(flow_metric)) +
  scale_x_continuous(name="Change in Delta H") +
  # theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  scale_y_continuous(name="Change in Probability")


### get mean and error

allDataMean <- allData %>% 
  group_by(Bio, flow_metric) %>%
  summarise(DeltaHMean = mean(DeltaH), BioMean = mean(na.omit((Change))))

allDataMean

