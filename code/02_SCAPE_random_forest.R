## random forest for SCAPE analysis

library(tidyverse)
library(tidyr)

# delta <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/00_Final_FFM_DeltaH_AH/SOC_deltaH_supp_final_12012021.csv")
# head(delta)

# deltaCur <-delta %>%
#   select(site, region, year, water_year_type, flow_metric, deltah_cur_ref_final) %>%
#   pivot_wider(names_from=flow_metric, values_from = deltah_cur_ref_final)
# 
# head(deltaCur)


asci <- read.csv("input_data/00_asci_delta_formatted_median_Nov2021.csv")
asci


