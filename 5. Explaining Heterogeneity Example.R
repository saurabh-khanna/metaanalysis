######################################################################
################### UMD - February 2020 ##############################
################### Explaining Heterogeneity #########################
################### BAI Example ######################################
######################################################################

#### February 20, 2020 ####

#### Load dataset ####

#load("BAIforYouth.rdata") # Function to load dataset
#dat_BAI <- BAIforYouth #Renames the dataset (as well as creating a new dataset with this name)
#View(dat_BAI) # This function allows you to see the dataset in R

dat_BAI <- read.csv("/home/saurabh/Everything/PhD/Workshops/Meta Analysis Course/BAIforYouth.csv", header = T)
View(dat_BAI) # This function allows you to see the dataset in R

#### Load packages ####

library(metafor)
library(robumeta)
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

### random-effects meta-analysis using REML estimator ###
re_BAI_sub0 <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML", 
              subset = dat_BAI$group == 0, 
              mods = rct)
summary(re_BAI_sub0)

### create subgroup datasets using "groups" variable ###

dat_BAI_g0 <- subset(dat_BAI, dat_BAI$group == 0)
dat_BAI_g1 <- subset(dat_BAI, dat_BAI$group == 1)

### random-effects meta-analysis with subgroup analysis using REML ###

re_BAI_g0 <- rma(yi = yi, vi = vi, data = dat_BAI_g0, method = "REML")
summary(re_BAI_g0)

re_BAI_g1 <- rma(yi = yi, vi = vi, data = dat_BAI_g1, method = "REML")
summary(re_BAI_g1)

### random-effects meta-analysis with anova-like moderator analysis using REML (binary variable) ###
re_BAI_mod1 <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML",
                 mods = group)
summary(re_BAI_mod1)

### random-effects meta-analysis with anova-like moderator analysis using REML (3 or more-levels variable) ###
re_BAI_mod2 <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML",
                      mods = ~ factor(pubyear))
summary(re_BAI_mod2)

### random-effects meta-analysis with meta-regression using multiple predictors###
re_ML_BAI_reg1 <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML",
                     mods = ~ factor(group) + scale(pubyear) + factor(rct))
summary(re_ML_BAI_reg1)

### random-effects with rve meta-regression using multiple predictors ###

rve_ML_BAI_reg1 <- robu(formula = yi ~ factor(group) + scale(pubyear) + factor(rct),
                        var.eff.size = vi,
                        studynum = studyid,
                        data = dat_BAI)
print(rve_ML_BAI_reg1)



#### Creating plots using rma ####

### create a funnel plot ###
re_BAI <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML")
summary(re_BAI)

funnel(re_BAI)

re_BAI_trim <- trimfill(re_BAI, side = "left")
summary(re_BAI_trim)
funnel(re_BAI_trim)

### create box plot ###
# run meta-regression analysis #
re_BAI_mod1 <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML",
                      mods = ~ factor(group) + scale(pubyear) -1)
summary(re_BAI_mod1)

# create two objects based on results, making numbers positive #
Non_Random <- re_BAI_mod1$b[1,1]*-1
Random <- re_BAI_mod1$b[2,1]*-1

# create a small dataframe for plotting #
modresults.ES <- c(Non_Random, Random)
modresults.label <- c("Non-Random", "Random")

modresults <- data.frame(modresults.label, modresults.ES)

# plot the results #
bar.plot <- ggplot(modresults, aes(modresults.label, modresults.ES, fill = modresults.label)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete("") + 
  ylab("Effect Size") +
  theme_bw() +
  theme(legend.position = "none")
bar.plot

### create a meta-analytic scatter plot ###
reg2plot <- ggplot(data = dat_BAI, aes(scale(postwks),yi)) + 
  geom_point(aes(size = 1/vi)) + 
  geom_smooth(method = lm) +
  xlab("Post weeks") +
  ylab("Effect Size") +
  theme_bw() +
  theme(legend.position = "none")
reg2plot

