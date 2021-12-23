## relative change in probability of achieving a good bio score

## packages

library(tidyverse)
library(tidyr)

out.dir <- "output_data/Manuscript/Figures/"

## full names for labels
labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(Hydro_endpoint = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels


### delta h limits

limits <- read.csv("output_data/00_ALL_delta_thresholds_scaled.csv")
limits

limits <- left_join(limits, labels, by = "Hydro_endpoint")

### SOC data (to change with RB9 data)

delta <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/00_Final_FFM_DeltaH_AH/SOC_deltaH_supp_final_12012021.csv")
head(delta)

## get delta h at each subbasin

delta_long <- delta %>%
  select(site, region, year, flow_metric, deltah_cur_ref_final, deltaH_watercon_ref_final) %>%
  pivot_longer(deltah_cur_ref_final:deltaH_watercon_ref_final, names_to = "Scenario", values_to = "DeltaH")

head(delta_long)

delta_med <- delta_long %>%
  group_by(site, region, flow_metric, Scenario) %>%
  summarise(DeltaHMed = median(na.omit(DeltaH)))

delta_med 


# ASCI --------------------------------------------------------------------

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
all_asci_sub <- subset(all_asci, hydro.endpoints %in% asci_metrics)

unique(all_asci_sub$hydro.endpoints)




