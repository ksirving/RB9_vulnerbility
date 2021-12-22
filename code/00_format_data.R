## Format data
## testing with SOC data, replace later

## packages

library(tidyverse)
library(tidyr)

## function
load("code/functions/root_interpolation_function.Rdata")

## regional curves delta H data
## upload and get delta h 


# CSCI --------------------------------------------------------------------


all_csci <- read.csv("input_data/01_csci_neg_pos_logR_metrics_figures_April2021.csv")

all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep=""))

all_csci <- left_join(all_csci, labels, by ="hydro.endpoints")

head(all_csci)

csci_metrics <-c("Q99", "SP_Tim","DS_Dur_WS")

## subset to only important metrics
all_csci_sub <- subset(all_csci, hydro.endpoints %in% csci_metrics)

# ASCI --------------------------------------------------------------------


head(asci)
all_asci <- read.csv("input_data/01_h_asci_neg_pos_logR_metrics_figures_April2021.csv")
head(all_asci)

## scale probability
all_asci <- all_asci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep=""))

all_asci <- left_join(all_asci, labels, by ="hydro.endpoints")


## subset to only important metrics
all_asci_sub <- subset(all_asci, hydro.endpoints %in% asci_metrics)

unique(all_asci_sub$hydro.endpoints)




# find roots of curve -----------------------------------------------------

## ASCI

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold25", "Threshold50", "Threshold75", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics
metrics <- unique(all_asci$comb_code_type)

metrics

## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- all_asci %>%
    filter(comb_code_type == met)
  
  ## get curves values at different probabilities
  thresh50 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.5)
  thresh50 <- ifelse(length(thresh50) == 0, NA, thresh50)
  
  thresh25 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.25)
  thresh25 <- ifelse(length(thresh25) == 0, NA, thresh25)
  
  thresh75 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.75)
  thresh75 <- ifelse(length(thresh75) == 0, NA, thresh75)
  
  ## add info to df
  df[i, 1] <- met
  df[i, 2] <- thresh25
  df[i, 3] <- thresh50
  df[i, 4] <- thresh75
  df[i, 5] <- length(hydroxx$PredictedProbabilityScaled)
  df[i ,6] <- hydroxx$Type[1]
  df[i ,7] <- "ASCI"
  df[i, 8] <- hydroxx$biol.endpoints[1]
  df[i, 9] <- hydroxx$thresholds[1]
  df[i, 10] <- hydroxx$hydro.endpoints[1]
  
  
}


df
write.csv(df, "output_data/00_ASCI_delta_thresholds_scaled.csv")


# CSCI --------------------------------------------------------------------

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold25", "Threshold50", "Threshold75", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics
metrics <- unique(all_csci$comb_code_type)

metrics

## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- all_csci %>%
    filter(comb_code_type == met)
  
  ## get curves values at different probabilities
  thresh50 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.5)
  thresh50 <- ifelse(length(thresh50) == 0, NA, thresh50)
  
  thresh25 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.25)
  thresh25 <- ifelse(length(thresh25) == 0, NA, thresh25)
  
  thresh75 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.75)
  thresh75 <- ifelse(length(thresh75) == 0, NA, thresh75)
  
  ## add info to df
  df[i, 1] <- met
  df[i, 2] <- thresh25
  df[i, 3] <- thresh50
  df[i, 4] <- thresh75
  df[i, 5] <- length(hydroxx$PredictedProbabilityScaled)
  df[i ,6] <- hydroxx$Type[1]
  df[i ,7] <- "CSCI"
  df[i, 8] <- hydroxx$biol.endpoints[1]
  df[i, 9] <- hydroxx$thresholds[1]
  df[i, 10] <- hydroxx$hydro.endpoints[1]
  
  
}


df
write.csv(df, "output_data/00_CSCI_delta_thresholds_scaled.csv")


# Combine data ------------------------------------------------------------

asci <- read.csv("output_data/00_ASCI_delta_thresholds_scaled.csv")
csci <- read.csv("output_data/00_CSCI_delta_thresholds_scaled.csv")

delta <- rbind(asci, csci)

write.csv(delta, "output_data/00_ALL_delta_thresholds_scaled.csv")



