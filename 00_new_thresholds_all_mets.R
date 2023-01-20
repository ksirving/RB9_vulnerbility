## Format data
## testing with SOC data, replace later

## packages

library(tidyverse)
library(tidyr)

## function
load("code/functions/root_interpolation_function.Rdata")

## regional curves delta H data
## upload and get delta h 


## full names for labels
labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(Hydro_endpoint = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels


# CSCI --------------------------------------------------------------------


all_csci <- read.csv("input_data/01_csci_neg_pos_logR_metrics_figures_April2021.csv")

all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep="")) 

all_csci <- left_join(all_csci, labels, by =c("hydro.endpoints" = "Hydro_endpoint"))

head(all_csci)

# csci_metrics <-c("Q99", "SP_Tim","DS_Dur_WS")

## subset to only important metrics
# all_csci_sub <- subset(all_csci, hydro.endpoints %in% csci_metrics)

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

all_asci <- left_join(all_asci,  labels, by =c("hydro.endpoints" = "Hydro_endpoint"))


## subset to only important metrics
# all_asci_sub <- subset(all_asci, hydro.endpoints %in% csci_metrics)

unique(all_asci_sub$hydro.endpoints)


# find roots of curve -----------------------------------------------------

## ASCI

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold70", "Threshold90", "Threshold99", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics
metrics <- unique(all_asci$comb_code_type)

metrics

## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- all_asci %>%
    filter(comb_code_type == met)
  
  ## get curves values at different probabilities
  thresh70 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.7)
  thresh70 <- ifelse(length(thresh70) == 0, NA, thresh70)
  
  thresh90 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.9)
  thresh90 <- ifelse(length(thresh90) == 0, NA, thresh90)
  
  thresh99 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.99)
  thresh99 <- ifelse(length(thresh99) == 0, NA, thresh99)
  
  ## add info to df
  df[i, 1] <- met
  df[i, 2] <- thresh70
  df[i, 3] <- thresh90
  df[i, 4] <- thresh99
  df[i, 5] <- length(hydroxx$PredictedProbabilityScaled)
  df[i ,6] <- hydroxx$Type[1]
  df[i ,7] <- "ASCI"
  df[i, 8] <- hydroxx$biol.endpoints[1]
  df[i, 9] <- hydroxx$thresholds[1]
  df[i, 10] <- hydroxx$hydro.endpoints[1]
  
  
}


df
write.csv(df, "output_data/00_ASCI_delta_thresholds_scaled_new_thresholds.csv")


# CSCI --------------------------------------------------------------------

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold70", "Threshold90", "Threshold99", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics
metrics <- unique(all_csci$comb_code_type)

metrics

## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- all_csci %>%
    filter(comb_code_type == met)
  
  ## get curves values at different probabilities
  thresh70 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.7)
  thresh70 <- ifelse(length(thresh70) == 0, NA, thresh70)
  
  thresh90 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.9)
  thresh90 <- ifelse(length(thresh90) == 0, NA, thresh90)
  
  thresh99 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.99)
  thresh99 <- ifelse(length(thresh99) == 0, NA, thresh99)
  
  ## add info to df
  df[i, 1] <- met
  df[i, 2] <- thresh70
  df[i, 3] <- thresh90
  df[i, 4] <- thresh99
  df[i, 5] <- length(hydroxx$PredictedProbabilityScaled)
  df[i ,6] <- hydroxx$Type[1]
  df[i ,7] <- "CSCI"
  df[i, 8] <- hydroxx$biol.endpoints[1]
  df[i, 9] <- hydroxx$thresholds[1]
  df[i, 10] <- hydroxx$hydro.endpoints[1]
  
  
}
  
  



df
write.csv(df, "output_data/00_CSCI_delta_thresholds_scaled_new_thresholds.csv")


# Combine data ------------------------------------------------------------

asci <- read.csv("output_data/00_ASCI_delta_thresholds_scaled_new_thresholds.csv")
csci <- read.csv("output_data/00_CSCI_delta_thresholds_scaled_new_thresholds.csv")

delta <- rbind(asci, csci)

write.csv(delta, "output_data/00_ALL_delta_thresholds_scaled_new_thresholds.csv")

head(delta)

delta <- delta %>%
  filter(Bio_threshold %in% c(0.86, 0.79)) %>%
  pivot_longer(Threshold70:Threshold99, names_to="ProbThreshold", values_to = "DeltaH") %>%
  select(-X,-n, -metric) %>%
  pivot_wider(names_from = Type, values_from = DeltaH) %>%
  select(Biol, Bio_threshold, Hydro_endpoint, ProbThreshold, Negative, Positive)

write.csv(delta, "output_data/00_ALL_delta_thresholds_scaled_new_thresholds_formatted.csv")

