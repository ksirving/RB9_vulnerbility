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

