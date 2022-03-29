### ASCI brts
getwd()

library(gbm)
library(dismo)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)

source("code/functions/My.gbm.step.R") ## function for brt - Ryan Peek

set.seed(321) # reproducibility

## upload data

data <- read.csv("input_data/00_asci_delta_formatted_median_Nov2021.csv")

## remove X column
data <- data[,-c(1, 7, 15)]

## remove diatom response & NAs
data <- select(data, - D_ASCI)
data <- data[!is.na(data$H_ASCI),]

## gbm functions in brt.functions script

## ASCI

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
hyper_grid

names(data)

# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 4, # response in training data
    gbm.x = 5:19, # hydro dat
    family = "gaussian",
    data = data,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data) 
)

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))


# based on above, run final BRT and save:
gbm_final_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 4, # response in training data
    gbm.x = 5:19, # hydro dat
    family = "gaussian",
    data = data,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

# set up filename for best model outputs
(gbm_best_file <- paste0("models/05_gbm_final_asci_model_output.txt"))

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write.csv(hyper_best, "output_data/05_best_model_asci_output.csv")

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 
# 0.3661235


# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_asci")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_asci")))

# save to RDS
write_rds(x = get(fileToSave), path = paste0("models/05_",fileToSave, "_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("models/05_",fileToSave,"_model_data.rda")))

gbm_final <- read_rds("models/05_gbm_final_asci_model.rds")
class(gbm_final)

gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) 
gbm_fin_RI  



# Plots and metrics-------------------------------------------------------------------

## upload full FFM names

labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(var = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels

## combine with rel importance
gbm_fin_RI <- left_join(gbm_fin_RI, labels, by ="var")

write.csv(gbm_fin_RI, "models/05_rel_imp_asci_labels.csv")

## plot
ggplot(data=gbm_fin_RI, aes(x=reorder(Flow.Metric.Name,-rel.inf), y=rel.inf, fill = Flow.Component)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  labs(title = "Relative Importance of FFM on ASCI",
       x = "Flow Metric",
       y = "Relative Importance (%)") #+ theme_bw(base_size = 15)

gbm_fin_RI

