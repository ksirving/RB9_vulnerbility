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
  select(-X.1, -X, -n) %>%
  # rename(hydro.endpoint = Hydro_endpoint) %>% 
  mutate(metric = paste0(Bio_endpoint, "_", Hydro_endpoint, "_", Bio_threshold)) %>%
  pivot_longer(Threshold25:Threshold75, names_to = "Threshold") %>%
  rename(DeltaH = value) 

## make wider with type - pos/neg
limits <- limits %>%
  pivot_wider(names_from = Type, values_from = DeltaH)

limits <- left_join(limits, labels, by = "Hydro_endpoint")

## filter out bad relationships

unique(limits$metric)

bad_rels <- c("H_ASCI_FA_Mag_0.75_Positive", "H_ASCI_FA_Mag_0.86_Positive", "H_ASCI_FA_Mag_0.94_Positive",
              "H_ASCI_FA_Mag_0.75_Negative", "H_ASCI_FA_Mag_0.86_Negative", "H_ASCI_FA_Mag_0.94_Negative", 
              "CSCI_DS_Mag_50_0.63_Positive", "CSCI_DS_Mag_50_0.63_Negative", "CSCI_DS_Mag_50_0.79_Positive", 
              "CSCI_DS_Mag_50_0.79_Negative", "CSCI_FA_Mag_0.79_Positive", "CSCI_FA_Mag_0.79_Negative", 
              "CSCI_FA_Mag_0.92_Positive", "CSCI_FA_Mag_0.92_Negative", "CSCI_Wet_BFL_Mag_50_0.79_Negative", 
              "CSCI_Wet_BFL_Mag_50_0.79_Positive")

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


# Join limits with delta H RB9 --------------------------------------------

limits <- limits  %>%
  # select(-X.1, -X) %>%
  rename(hydro.endpoint = Hydro_endpoint)

## join limits with delta data
delta_df <- left_join(delta_long, limits, by="hydro.endpoint")
head(delta_df)

## define alteration per subbasin, per year - within limits
delta_dfx <- delta_df %>%
  group_by(comid, comid_wy, wayr, hydro.endpoint, Bio_endpoint,Bio_threshold, Threshold) %>%
  mutate(Alteration = ifelse(DeltaH <= Positive & DeltaH >= Negative, "Unaltered", "Altered")) 

write.csv(delta_dfx, "ignore/04_alteration_by_year_comid_all_sites.csv")

head(delta_dfx)

# Comparison of thresholds ------------------------------------------------
delta_dfx <- na.omit(delta_dfx)
unique(delta_dfx$Biol)
## get number and percentage of sites per year altered and combination code

Year_Tally <- delta_dfx %>%
  select(comid, comid_wy, wayr, hydro.endpoint, Biol ,Bio_threshold, Threshold, Alteration) %>%
  mutate(CombCode = paste0(Bio_threshold, "_", Threshold )) %>%
  group_by(wayr, hydro.endpoint, Biol, Bio_threshold, Threshold, CombCode) %>%
  count(Alteration) %>%
  mutate(Percentage = n/sum(n)*100) %>%  select(-n) %>%
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

Tally <- Year_Tally %>%
  group_by(CombCode, hydro.endpoint, Biol) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) 

write.csv(Tally, "output_data/04_nhd_alteration_median_mean.csv")

unique(Year_Tally$hydro.endpoint)

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), hydro.endpoint == "DS_Mag_50")

head(Tally0)


csci1 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 40, ymax = 60, xmin = 1994, xmax = 2018,
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

###################
## NONE!!!!
## only relationship good is 0.92 which isn't in the med alteration range
#################

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), hydro.endpoint == "FA_Mag")

head(Tally0)

csci2 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
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
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
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
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
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
  annotate("rect", ymin = 40, ymax = 60, xmin = 1994, xmax = 2018,
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

###################
## 0.75_Threshold25 
#################

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), hydro.endpoint == "FA_Mag")

head(Tally0)

ASCI2 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
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
  annotate("rect", ymin = 40, ymax = 60, xmin = 1994, xmax = 2018,
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
##############
## find metrics within limits



Tally0 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), hydro.endpoint == "Wet_BFL_Mag_10" )

head(Tally0)

ASCI2 <- ggplot(data=Tally0, aes(x = wayr, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Wet Season Baseflow", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

ASCI2

out.filename <- paste0(out.dir,"ASCI_WS_baseflow_10_Alt_over_time.jpg")
ggsave(ASCI2, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 40 & AlteredMean < 60)
Tally0x
unique(Tally0x$CombCode) 

###   CombCode         AlteredMean AlteredMedian
#   1 0.75_Threshold25        45.4          44.7
# 2 0.75_Threshold50        56.7          54.0
# 3 0.94_Threshold25        59.3          57.6

## 0.75_Threshold25  

# Format data for map -----------------------------------------------

unique(Year_Tally$hydro.endpoint)

# CSCI -  0.79_Threshold75 (DS_Mag_90, Wet_BFL_Mag_10)
# ASCI - 0.75_Threshold25 (DS_Mag_90, Wet_BFL_Mag_10)

head(delta_dfx_sub)

## count alteration by site from main delta df
metric_tally_current <- delta_dfx %>%
  group_by(comid, hydro.endpoint, Biol, Threshold, Bio_threshold) %>%
  count(Alteration)

metric_tally_current <- na.omit(metric_tally_current)
metric_tally_current


## make %
metric_tally_current <- metric_tally_current %>%
  group_by(comid, hydro.endpoint, Biol, Threshold, Bio_threshold) %>%
  mutate(Percentage = n/sum(n)*100, YearsWithData = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration, values_from = Percentage)

metric_tally_current

## replace NAs with zero - as 100% in other category
metric_tally_current[is.na(metric_tally_current)] <- 0

delta_dfx_sub <- metric_tally_current%>%
  filter(hydro.endpoint %in% c("DS_Mag_90", "Wet_BFL_Mag_10"), 
         Bio_threshold %in% c(0.79, 0.75)) %>%
  mutate(threshCode = paste(Bio_threshold, Threshold, sep= "_")) %>%
  filter(threshCode %in% c("0.79_Threshold75", "0.75_Threshold25"))

unique(delta_dfx_sub$threshCode)

write.csv(delta_dfx_sub, "ignore/04_metric_suitability_tally_condensed_all_sites_current.csv")
