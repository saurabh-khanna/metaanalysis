######################################################################
################### UMD - February 2020 ##############################
################### Publication Bias##################################
################### BAI Example ######################################
######################################################################

#### February 20, 2020 ####

#### Load dataset ####

#load("BAIforYouth.rdata") # Function to load dataset
#dat_BAI <- BAIforYouth #Renames the dataset (as well as creating a new dataset with this name)
#View(dat_BAI) # This function allows you to see the dataset in R

dat_BAI <- read.csv("BAIforYouth.csv", header = T)
View(dat_BAI) # This function allows you to see the dataset in R


#### Load packages ####

library(metafor)
library(ggplot2)


#### Calculate effect sizes using metafor ####

dat_BAI <- escalc(measure = "SMD", # Tells R to calculate standardized mean-difference (Hedges' g)
                  m1i = mean_tx, m2i = mean_ct, # Treatment and control means
                  sd1i = sd_tx, sd2i = sd_ct, # Treatment and control SDs
                  n1i = n_tx_ob, n2i = n_ct_ob, # Treatment and control sample size
                  data = dat_BAI, # Dataset
                  append = TRUE) # Tell R to put ES and var in current dataset

View(dat_BAI)


#### Calculate meta-analysis models ####

### random-effects meta-analysis using method-of-moments estimator ###
re_REML_BAI <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML")
summary(re_REML_BAI)

### create funnel plot ###
funnel(re_REML_BAI)

### conduct egger regression test ###
regtest(re_REML_BAI, model = "rma", predictor = "sei")

### conduct trim and fill ###
re_REML_BAI_tf <- trimfill(re_REML_BAI, side = "left") # run the trim and fill function on rma object
funnel(re_REML_BAI_tf)
summary(re_REML_BAI_tf)

### create a forest plot ###

forest(re_REML_BAI)


### create various meta-analytic plots in one figure ###
plot(re_REML_BAI)
