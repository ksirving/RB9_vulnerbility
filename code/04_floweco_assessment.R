### current conditions assessment

## use flow-ecology curves

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
## check relationsionship with RB9 metrics
## get limits
## apply alteration rules on RB9 data


### delta h limits

limits <- read.csv("output_data/00_ALL_delta_thresholds_scaled.csv")
limits
head(limits)
## clean up df, make longer
limits <- limits %>%
  dplyr::select(-X.1, -X, -n) %>%
  # rename(hydro.endpoint = Hydro_endpoint) %>% 
  mutate(metric = paste0(Biol, "_", Hydro_endpoint, "_", Bio_threshold)) %>%
  pivot_longer(Threshold25:Threshold75, names_to = "Threshold") %>%
  rename(DeltaH = value) 


## make wider with type - pos/neg
limits <- limits %>%
  pivot_wider(names_from = Type, values_from = DeltaH)

limits <- left_join(limits, labels, by = "Hydro_endpoint")

## filter out bad relationships

unique(limits$metric)

bad_rels <- c("ASCI_FA_Mag_0.75", "ASCI_FA_Mag_0.86", "ASCI_FA_Mag_0.94",
              "ASCI_FA_Mag_0.75", "ASCI_FA_Mag_0.86", "ASCI_FA_Mag_0.94", 
              "CSCI_DS_Mag_50_0.63", "CSCI_DS_Mag_50_0.63", "CSCI_DS_Mag_50_0.79", 
              "CSCI_DS_Mag_50_0.79", "CSCI_FA_Mag_0.79", "CSCI_FA_Mag_0.79", 
              "CSCI_FA_Mag_0.92", "CSCI_FA_Mag_0.92", "CSCI_Wet_BFL_Mag_50_0.79", 
              "CSCI_Wet_BFL_Mag_50_0.79")

limits <- limits %>%
  filter(!metric %in% bad_rels)


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



# RB9 delta ---------------------------------------------------------------

delta <- read.csv("ignore/2022-02-11_RFpred_output_alldata_SDCOMIDs.csv")
head(delta)
dim(delta)

## remove duplicates
delta <- delta %>% distinct()

delta_long <- delta %>%
  # select(comid region, year, flow_metric, deltah_cur_ref_final, deltaH_watercon_ref_final) %>%
  pivot_longer(d_ds_dur_ws:d_wet_tim, names_to = "FlowMetric", values_to = "DeltaH")

head(delta_long)

## keep only high R2s
# fa_mag
# wet_bfl_mag_10
# peak_10
# ds_mag_90
# peak_2
# peak_5
# ds_mag_50

unique(delta_long$FlowMetric)

delta_long <- delta_long %>%
  filter(FlowMetric %in% c("d_fa_mag", "d_wet_bfl_mag_10", "d_ds_mag_90", "d_ds_mag_50")) %>%
  mutate(hydro.endpoint = case_when(FlowMetric == "d_ds_mag_50" ~ "DS_Mag_50",
                                     FlowMetric == "d_ds_mag_90" ~ "DS_Mag_90",
                                     FlowMetric == "d_fa_mag" ~ "FA_Mag",
                                     # FlowMetric == "d_peak_10" ~ "DS_Mag_50",
                                     # FlowMetric == "d_peak_2" ~ "DS_Mag_50",
                                     # FlowMetric == "d_peak_5" ~ "DS_Mag_50",
                                     # FlowMetric == "sp_mag" ~ "SP_Mag",
                                     FlowMetric == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     FlowMetric == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50",)) 
names(delta_long)
## get median delta H 

delta_med <- delta_long %>%
  group_by(comid, FlowMetric, hydro.endpoint) %>%
  summarise(MedDelta = median(DeltaH))

head(delta_med)

# Join limits with delta H RB9 --------------------------------------------

limits <- limits  %>%
  # dplyr::select(-X.1, -X) %>%
  rename(hydro.endpoint = Hydro_endpoint)

## join limits with delta data
delta_df <- left_join(delta_long, limits, by="hydro.endpoint")
head(delta_df)

## define alteration per subbasin, per year - within limits
delta_dfx <- delta_df %>%
  group_by(comid,comid_wy, wayr,  hydro.endpoint, Bio_endpoint,Bio_threshold, Threshold) %>%
  mutate(Alteration = ifelse(DeltaH <= Positive & DeltaH >= Negative, "Unaltered", "Altered")) 

write.csv(delta_dfx, "ignore/04_alteration_by_year_comid_all_sites.csv")

head(delta_dfx)

# Comparison of thresholds ------------------------------------------------
delta_dfx <- na.omit(delta_dfx)
unique(delta_dfx$Biol)
## get number and percentage of sites per year altered and combination code

Year_Tally <- delta_dfx %>%
  dplyr::select(comid, comid_wy, wayr, hydro.endpoint, Biol ,Bio_threshold, Threshold, Alteration) %>%
  mutate(CombCode = paste0(Bio_threshold, "_", Threshold )) %>%
  group_by(wayr, hydro.endpoint, Biol, Bio_threshold, Threshold, CombCode) %>%
  count(Alteration) %>%
  mutate(Percentage = n/sum(n)*100) %>%  dplyr::select(-n) %>%
  pivot_wider(names_from = Alteration, values_from = Percentage)

view(Year_Tally)

Year_Tally[is.na(Year_Tally)] <- 0

head(Year_Tally)

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI","ASCI"))

# CSCI -----------------------------------------------------------------

getwd()
out.dir <- "figures/"

# format thresholds
Year_Tally$Bio_threshold <- as.character(Year_Tally$Bio_threshold)

range(Year_Tally$wayr)

Tally <- Year_Tally %>%
  group_by(CombCode, hydro.endpoint, Biol) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) 

write.csv(Tally, "output_data/04_nhd_alteration_median_mean.csv")

unique(Year_Tally$hydro.endpoint)

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), hydro.endpoint == "DS_Mag_50")

head(Tally0)


csci1 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 2000, xmax = 2014,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Dry Season Baseflow", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_colour_discrete(name  ="CSCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))


csci1

out.filename <- paste0(out.dir,"CSCI_DS_Baseflow_Alt_over_time.jpg")
ggsave(csci1, file = out.filename, dpi=300, height=4, width=6)



## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode,  hydro.endpoint) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 40 & AlteredMean < 60)
Tally0x
unique(Tally0x$CombCode)


## NONE!!!!
## only relationship good is 0.92 which isn't in the med alteration range


Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), hydro.endpoint == "FA_Mag")

head(Tally0)

csci2 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 2000, xmax = 2014,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Fall Magnitude", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="CSCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

csci2

out.filename <- paste0(out.dir,"CSCI_FA_mag_Alt_over_time.jpg")
ggsave(csci2, file = out.filename, dpi=300, height=4, width=6)


## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally0x
unique(Tally0x$CombCode) 

# NONE!

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), hydro.endpoint == "DS_Mag_90")

head(Tally)

csci2 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 2000, xmax = 2014,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Dry Season Baseflow High", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="CSCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

csci2

out.filename <- paste0(out.dir,"CSCI_DS_Mag90_Alt_over_time.jpg")
ggsave(csci2, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally0x
unique(Tally0x$CombCode) 

# 0.79_Threshold50        27.0          20.2
# 2 0.79_Threshold75        43.1          36.5


Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), hydro.endpoint == "Wet_BFL_Mag_10")

head(Tally0)

csci2 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 2000, xmax = 2014,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Wet Season Baseflow", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="CSCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

csci2

out.filename <- paste0(out.dir,"CSCI_WS_baseflow_10_Alt_over_time.jpg")
ggsave(csci2, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 40 & AlteredMean < 60)
Tally0x
unique(Tally0x$CombCode) 

# CombCode         AlteredMean AlteredMedian
# <chr>                  <dbl>         <dbl>
#   1 0.63_Threshold25        40.5          37.2
# 2 0.63_Threshold50        48.0          45.0
# 3 0.63_Threshold75        56.4          52.8
# 4 0.79_Threshold25        40.9          37.3
# 5 0.79_Threshold50        48.6          45.7
# 6 0.79_Threshold75        56.9          53.5
# 7 0.92_Threshold25        59.3          56.6


## 0.79_Threshold75 in DS_Mag_90 and WS_basfl_10 - closest to median = 50

# ASCI-------------------------------------------------------------------

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), hydro.endpoint == "DS_Mag_90")

head(Tally0)


ASCI1 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 2000, xmax = 2014,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Dry Season Baseflow High", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))


ASCI1

out.filename <- paste0(out.dir,"ASCI_DS_Baseflow_90_Alt_over_time.jpg")
ggsave(ASCI1, file = out.filename, dpi=300, height=4, width=6)



## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode,  hydro.endpoint) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally0x
unique(Tally0x$CombCode)


## 0.75_Threshold25 


Tally0 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), hydro.endpoint == "FA_Mag")

head(Tally0)

ASCI2 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 2000, xmax = 2014,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Fall Magnitude", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

ASCI2

out.filename <- paste0(out.dir,"ASCI_FA_mag_Alt_over_time.jpg")
ggsave(ASCI2, file = out.filename, dpi=300, height=4, width=6)


## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally0x
unique(Tally0x$CombCode) 

# NONE!!!! bad relationship

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), hydro.endpoint == "DS_Mag_50")

head(Tally0)


ASCI1 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 2000, xmax = 2014,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Dry Season Baseflow", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))


ASCI1

out.filename <- paste0(out.dir,"ASCI_DS_Baseflow_Alt_over_time.jpg")
ggsave(ASCI1, file = out.filename, dpi=300, height=4, width=6)



## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode,  hydro.endpoint) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally0x
unique(Tally0x$CombCode)


#  0.75_Threshold25 DS_Mag_50             73.3          75.4
#  0.94_Threshold25 DS_Mag_50             73.7          76.8

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), hydro.endpoint == "Wet_BFL_Mag_10")

head(Tally0)


ASCI1 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 2000, xmax = 2014,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Wet Season Baseflow", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))


ASCI1

out.filename <- paste0(out.dir,"ASCI_Wet_BFL_Mag_10_Alt_over_time.jpg")
ggsave(ASCI1, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode,  hydro.endpoint) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally0x
unique(Tally0x$CombCode)

# 1 0.75_Threshold25 Wet_BFL_Mag_10        45.4          44.7
# 2 0.75_Threshold50 Wet_BFL_Mag_10        56.7          54.0
# 3 0.75_Threshold75 Wet_BFL_Mag_10        72.9          68.9
# 4 0.86_Threshold25 Wet_BFL_Mag_10        62.4          59.1
# 5 0.94_Threshold25 Wet_BFL_Mag_10        59.3          57.6
# 6 0.94_Threshold50 Wet_BFL_Mag_10        74.7          72.5

# Format data for map -----------------------------------------------

unique(Year_Tally$hydro.endpoint)

# CSCI -  0.79_Threshold75 (DS_Mag_90, Wet_BFL_Mag_10)
# ASCI - 0.75_Threshold25 (DS_Mag_90, Wet_BFL_Mag_10)

# head(delta_dfx_sub)
head(delta_med)

## join limits with delta data median
delta_df <- left_join(delta_med, limits, by="hydro.endpoint")
head(delta_df)

## define alteration per subbasin, per year - within limits
delta_dfx <- delta_df %>%
  group_by(comid,  hydro.endpoint, Bio_endpoint,Bio_threshold, Threshold) %>%
  mutate(Alteration = ifelse(MedDelta <= Positive & MedDelta >= Negative, "Unaltered", "Altered")) 

write.csv(delta_dfx, "ignore/04_alteration_by_median_comid_all_sites.csv")

head(delta_dfx)

## count alteration by site from main delta df
# metric_tally_current <- delta_dfx %>%
#   group_by(comid, hydro.endpoint, Biol, Threshold, Bio_threshold) %>%
#   count(Alteration)
# 
# metric_tally_current <- na.omit(metric_tally_current)
# metric_tally_current


## make %
# metric_tally_current <- metric_tally_current %>%
#   group_by(comid, hydro.endpoint, Biol, Threshold, Bio_threshold) %>%
#   mutate(Percentage = n/sum(n)*100, YearsWithData = sum(n)) %>%
#   select(-n) %>%
#   pivot_wider(names_from = Alteration, values_from = Percentage)
# 
# metric_tally_current %>%   tally() 

## replace NAs with zero - as 100% in other category
# metric_tally_current[is.na(metric_tally_current)] <- 0

delta_dfx_sub <- delta_dfx %>%
  filter(hydro.endpoint %in% c("DS_Mag_90", "Wet_BFL_Mag_10"), 
         Bio_threshold %in% c(0.79, 0.75)) %>%
  mutate(threshCode = paste(Bio_threshold, Threshold, sep= "_")) %>%
  filter(threshCode %in% c("0.79_Threshold75", "0.75_Threshold25"))

unique(delta_dfx_sub$threshCode)

write.csv(delta_dfx_sub, "ignore/04_metric_suitability_tally_condensed_all_sites_current.csv")


# Alteration criteria ----------------------------------------------------------------

delta_dfx_sub <- read.csv("ignore/04_metric_suitability_tally_condensed_all_sites_current.csv")

head(delta_dfx_sub)

library(sf)
library(raster)
library(maptools)
library(rgdal)

## projection
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

## set metric alteration based on median alteration
#  if 1 or 2 metrics altered then bio is altered
# if both bio altered = altered, of 1 bio altered = partially altered, if 0 bio altered = unaltered

## change names
suit_data <- delta_dfx_sub %>%
  rename(COMID = comid)
head(suit_data)

# aggregate by site, Biol, Threshold, alteration_50pct_time, get a count of n of altered and unaltered metrics for each
subset.50pct.time <- suit_data %>% 
  group_by(COMID, Biol, Threshold, Alteration) %>% 
  tally() %>% 
  ungroup() %>% 
  data.frame()

subset.50pct.time

# if 1-2 metric altered, bio index is considered altered
# if number altered > 1 --> class it as overall altered
subset.50pct.time$overall.altered.2metric <- NA #create a new column as NA

# if >=1 metric altered, save as altered, all others unaltered, then any unaltered with 1 metric also altered
subset.50pct.time <- subset.50pct.time %>%
  mutate(overall.altered.2metric = ifelse(Alteration == "Altered" & n >= 1, "Altered", "Unaltered")) %>%
  mutate(overall.altered.2metric = ifelse(overall.altered.2metric == "Unaltered" & n == 1, "Altered", overall.altered.2metric)) %>%
  dplyr::select(-Alteration) %>%
  distinct()

# save csv in output_data
file.name <- "ignore/04_summary.50pct.time.altered.2metrics.Current.csv"
write.csv(subset.50pct.time, file=file.name, row.names = FALSE)

subset.50pct.time

# remove NA rows (duplicate rows) and remove calulation columns
separate.summary <- subset.50pct.time %>%
  na.omit() %>%
  dplyr::select(-Threshold, -n)

## Summarize overall alteration across rb9
# number of altered and unaltered subbasins, percent of total subbasins using these thresholds

# find length of unique sites
site.length <- length(unique(subset.50pct.time$COMID))
site.length

##### something is wrong here!!!!!
# summarize (count and %) of total subbasin in overall alteration categories
subset.50pct.time.summary2 <- subset.50pct.time %>% 
  group_by(Biol, Threshold, overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/site.length) %>% 
  data.frame()

subset.50pct.time.summary2

# Biol   Threshold overall.altered.2metric    n pct.2metric
# 1 ASCI Threshold25                 Altered 1461    69.04537
# 2 ASCI Threshold25               Unaltered  655    30.95463
# 3 CSCI Threshold75                 Altered 1368    64.65028
# 4 CSCI Threshold75               Unaltered  748    35.34972


# Overall synthesis -------------------------------------------------------

## Create overall prioritization based on bio-relevant alteration of CSCI and ASCI
# if both altered, high priority; if one altered, medium priority; if unaltered, low priority

# remove NA values
subset.50pct.time.all2 <- na.omit(subset.50pct.time)

# tally number of biol indices (csci/asci) altered per site
subset.50pct.time.summary2.all <- subset.50pct.time.all2 %>% 
  group_by(COMID,  overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit()  

# save as overall.summary
overall.summary <- data.frame(subset.50pct.time.summary2.all)
overall.summary
# create new column for synthesis alteration, blank with NA values
overall.summary$synthesis_alteration <- NA
# designation prioritization categories
# if 2 bio indices altered, high priority
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Altered" & overall.summary$n == 2)] <- "High Priority" 
# if one is unaltered, medium
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Unaltered" & overall.summary$n == 1)] <- "Medium Priority" 
# if 2 bio indices unaltered, low priority
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Unaltered" & overall.summary$n == 2)] <- "Low Priority" 

# remove NA rows (duplicate rows) and remove calulation columns
synthesis.summary <- overall.summary %>%
  na.omit() %>%
  dplyr::select(-overall.altered.2metric, -n)

dim(synthesis.summary)
#summary
synthesis.summary.table <- synthesis.summary %>% 
  na.omit()  %>% 
  group_by(synthesis_alteration) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(pct.2metric = 100*n/sum(n)) 

synthesis.summary.table <- data.frame(synthesis.summary.table)
synthesis.summary.table


# Maps --------------------------------------------------------------------
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

library(spDataLarge)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(ggspatial)
library(spData)      
library(geosphere)
library(rgeos)
### upload RB9 nhds

calinhd <- readOGR('/Users/katieirving/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
unique(calinhd$COMID)
unique(overall.summary$COMID)


## synthesis map

# fortified calinhd, joind with delta
# merge synthesis.summary with basins to get spatial data
SynthNHD <- calinhd %>%
  filter(COMID %in% unique(synthesis.summary$COMID)) %>%
  dplyr::select(COMID) %>% ## 46
  as('Spatial') %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE)

nhdplo <- SynthNHD %>%
  fortify %>%
  # left_join(comidid, by = 'id') %>%
  full_join(synthesis.summary, by = 'COMID')


# colors and labels for suitability categories - used for legend and maps
colors <- c("#ca0020", "#fdae61","#0571b0", "white")
priority <- c("High Priority",  "Medium Priority","Low Priority", NA)
categories <- c("High (Alteration: CSCI & ASCI)", "Medium (Alteration: CSCI or ASCI)","Low (Alteration: None)","Not evaluated")

# lookup table for colors and categories for legend and maps
lookup <- data.frame(cbind(colors, priority, categories))

## plot
# Set up base map 

study <- ggplot(SynthNHD) + 
  labs(title="Current Condition", subtitle = "Based on Biologically-Relevant Flow Alteration",x ="", y = "")  + 
  geom_sf(color = "lightgrey", fill="white") +
  annotation_scale() +
  annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         width = unit(.8, "cm")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8)) 

# print basemap
study

## subset lookup categories and tables
lookup.sub <- lookup[lookup$priority %in% unique(nhdplo$synthesis_alteration),]

# save as factor for legend ordering
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
nhdplo$synthesis_alteration <- factor(nhdplo$synthesis_alteration, levels = unique(lookup.sub$priority))

# synthesis map for bio index z
syn.plot <- study + geom_sf(data = nhdplo, aes(color=synthesis_alteration, geometry = geometry)) +
  scale_color_manual(name = "Condition based on Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) 

# print map
print(syn.plot)

# write plot
out.filename <- "figures/maps/04_Synthesis_Prioritization_map_RB9_Current.jpg"
ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)

# CSCI & ASCI maps --------------------------------------------------------

separate.summary

## Create bio-relevant flow alteration CSCI and ASCI maps 
# for appropriate prob and biol threshold combos, altered dependent on mdeian, altered 1 or 2 metrics

# set colors for alteration categories used in legend and maps
colors <- c("#ca0020", "#0571b0", "white")
alteration <- c("Altered",  "Unaltered", NA)
categories <- c("Likely Altered", "Likely Unaltered", "Not evaluated")
# lookup table used for legend and maps
lookup <- data.frame(cbind(colors, alteration, categories))

# create title for plot (metric threshold)
metric.threshold <- "2 Metric Altered Threshold"

# subset to specific columns, rename columns
subset <- separate.summary %>% 
  dplyr::select("COMID", "Biol","overall.altered.2metric") %>% 
  # mutate(COMID = as.character(COMID)) %>% 
  data.frame() %>% 
  na.omit()

# update column names
names(subset) <- c("COMID", "Biol","Alteration - Biology")

## Loop through CSCI and ASCI thresholds
indices <- c("ASCI", "CSCI")

for(z in indices){
  #subset either csci or asci
  subset.index <- subset[subset$Biol == z,]
  subset.index
  # set probability threshold label
  # prob <- "Probability Threshold at 25%"
  
  # merge with basins to get spatial data for map
  subset.join <- subset.index %>% 
    full_join(SynthNHD, by = c('COMID'))
  subset.join
  # set title and subtitle
  title <- paste0(z)
  subtitle <- "Biologically-Relevant Flow Alteration"
  
  ## Plot
  # Set up base map 
  study <- ggplot(SynthNHD) + 
    geom_sf(color = "lightgrey", fill="white") +
    labs(title=title, subtitle = subtitle, x ="", y = "")  + 
    annotation_scale() +
    annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                           width = unit(.8, "cm")) +
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8),
          plot.title = element_text(size=20)) 
  #print map
  study
  
  # subset lookup categories and tables
  lookup.sub <- lookup[lookup$alteration %in% unique(subset.join$`Alteration - Biology`),]
  
  # save as factor to sort categories in legend
  lookup.sub$alteration <- factor(lookup.sub$alteration, levels = unique(lookup.sub$alteration))
  subset.join$`Alteration - Biology` <- factor(subset.join$`Alteration - Biology`, levels = unique(lookup.sub$alteration))
  
  
  # synthesis map for bio index z
  syn.plot <- study + geom_sf(data = subset.join, aes(color=`Alteration - Biology`, geometry = geometry)) +
    scale_color_manual(name = "Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) 
  
  # print
  print(syn.plot)
  
  # write plot
  out.filename <- paste0("figures/maps/04_", z, "_alteration_map_RB9_Current.jpg")
  ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
  
}


# rename Alteration categories - Bio to likely unaltered and likely altered categories
subset$`Alteration - Biology` <- gsub("Altered", "Likely Altered", subset$`Alteration - Biology`)
subset$`Alteration - Biology` <- gsub("Unaltered", "Likely Unaltered", subset$`Alteration - Biology`)

# save the subset summary table with indices, subbasin
# pivot wider to get hydro.alteration.CSCI and hydro.alteration.ASCI columns
subset2 <- subset %>% 
  # select(-c(Probability_Threshold)) %>% 
  pivot_wider(names_from = Biol, values_from = `Alteration - Biology`) %>% 
  rename(hydro.alteration.CSCI = CSCI) %>% 
  rename(hydro.alteration.ASCI = ASCI)

# remove na rows from overall summary
overall.summary2 <- na.omit(overall.summary)

# combine with overall summary
summary.csci.asci.synthesis <- subset2 %>% 
  inner_join(overall.summary2, by = c('COMID')) %>% 
  dplyr::select(c(names(subset2), "synthesis_alteration"))

# write csv summary for CSCI and ASCI
file.name.summary <- "output_data/04_RB9_CSCI_ASCI_HydroAlt_Synthesis_Summary_Current.csv"
write.csv(summary.csci.asci.synthesis, file = file.name.summary)

### tally
data <- read.csv("output_data/04_RB9_CSCI_ASCI_HydroAlt_Synthesis_Summary_Current.csv")

head(data)



table(data$hydro.alteration.ASCI)
# Likely Altered Likely Unaltered 
# 1461              655 

table(data$hydro.alteration.CSCI)
# Likely Altered Likely Unaltered 
# 1368              748

table(data$synthesis_alteration)

# High Priority    Low Priority Medium Priority 
# 1133             420             563 

data_med <- data %>%
  filter(synthesis_alteration == "Medium Priority")

sum(data_med$hydro.alteration.ASCI == "Likely Altered") # 328

sum(data_med$hydro.alteration.CSCI == "Likely Altered") # 235




