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


csci <- read.csv("input_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
csci

## work flow 
## get COMIDs so can predict on NHD reach
## get flow lines - may come with FMM data?




# Quantile Random Forest --------------------------------------------------

install.packages("quantregForest")
library(quantregForest)

set.seed(16)

## format data

## remove fall metrics as too many NAs
head(csci)

csci <- csci %>%
  select(csci, DS_Dur_WS:DS_Tim, Q99:Wet_Tim)

## remove observations with mising values
csci <- csci[ !apply(is.na(csci), 1,any), ]
csci
dim(csci)

## number of remining samples
n <- nrow(csci)
n

## divide into training and test data
indextrain <- sample(1:n,round(0.6*n),replace=FALSE)
indextrain

names(csci)

Xtrain     <- csci[ indextrain,2:14]
Xtest      <- csci[-indextrain,2:14]
Ytrain     <- csci[ indextrain,1]
Ytest      <- csci[-indextrain,1]

Xtrain
qrf <- quantregForest(x=Xtrain, y=Ytrain)
qrf
qrf <- quantregForest(x=Xtrain, y=Ytrain, nodesize=10,sampsize=30)
summary(qrf)

qrf$rsq
qrf$importance
qrf$mse

conditionalQuantiles  <- predict(qrf, Xtest, what=c(0.1,0.5,0.9))
conditionalQuantiles

## class from Beck et al 2018
# Class
# Likely unconstrained Possibly unconstrained
# Possibly constrained Likely constrained
# Definition
# Lower bound of prediction range is above threshold
# Lower bound of prediction range is below threshold, but median prediction is above
# Upper bound of prediction range is above threshold, but median prediction is below
# Upper bound of prediction range is below threshold
# Example
# 10th percentile > 0.79
# 50th percentile > 0.79 50th percentile < 0.79 90th percentile < 0.79

library(tidyr)
library(caret)
library(quantregForest)
library(ggplot2)
library(ggridges)
# install.packages("ggridges")
# install.packages("caret")

head(mtcars)
qrf<- train(mpg ~ cyl + disp + hp + drat + wt, data = mtcars, method = 'qrf')  # train QRF model
n_cars = 8
class(qrf$finalModel)

class(qrf$finalModel) <- 'randomForest'
pred <- predict(qrf$finalModel, mtcars[1:n_cars,], predict.all = T)
str(pred)
head(pred)

# reshape individual tree predictions for ggplot
obs <- mtcars$mpg[1:n_cars]
car <- row.names(mtcars)[1:n_cars]
tree_pred <- as.data.frame(pred$individual)
tree_pred
colnames(tree_pred) <- 1:500
tree_pred$obs <- obs
tree_pred$car <- car
tree_pred_long <- gather(tree_pred, tree, pred, 1:500)

pred$aggregate

# reshape aggregate predictions for ggplot
agg <- aggregate(pred ~ obs + car, data = tree_pred_long, FUN = mean)  # same as pred$aggregate values
colnames(agg) <- c('obs', 'car', 'mean_pred')
pred_obs <- data.frame(pred = pred$aggregate, obs = obs)
pred_obs <- merge(pred_obs, agg, by = 'obs')

# plot distribution of tree predictions and aggregate prediction of final QRF model
ggplot(tree_pred_long) + 
  geom_abline(slope = 1, intercept = 0, lty = 2)+  # line of identity
  geom_density_ridges(aes(x = pred, y = obs, group = car, fill = car),
                      alpha = 0.3, color = 'black', rel_min_height = 0.02, size = 0.5)+
  geom_point(data = pred_obs, aes(x = pred, y = obs, fill = car), pch = 21, size = 2)+
  theme_classic()+
  ggtitle('Seeing the (Random) Forest for the Trees')+
  coord_fixed(xlim = c(12,28), ylim = c(12,28))+
  scale_y_continuous('Observed MPG', breaks = seq(15,30,5))+
  scale_x_continuous('Predicted MPG', breaks = seq(15,30,5))
