################### Effect Size Synthesis ############################

#### Load packages ####
library(tidyverse)
library(metafor)
library(robumeta)

#### Load dataset ####

dat_BAI <- read.csv("/home/saurabh/Everything/PhD/Workshops/Meta Analysis Course/my_analysis/BAIforYouth.csv", header = T)
df <- read_csv("/home/saurabh/Everything/PhD/Workshops/Meta Analysis Course/my_analysis/rebecca_template.csv")

#### Calculate effect sizes using metafor ####

dat_BAI <- 
  escalc(
    measure = "SMD", # Tells R to calculate standardized mean-difference (Hedges' g)
    m1i = mean_tx, m2i = mean_ct, # Treatment and control means
    sd1i = sd_tx, sd2i = sd_ct, # Treatment and control SDs
    n1i = n_tx_ob, n2i = n_ct_ob, # Treatment and control sample size
    data = dat_BAI, # Dataset
    append = TRUE
  ) # Tell R to put ES and var in current dataset

View(dat_BAI)


#### Calculate meta-analysis models ####

### fixed-effect ###
fe_BAI <- rma(yi = yi, vi = vi, data = dat_BAI, method = "FE")
# yi = effect size, vi = variance, data = dataset, method = specify MA model
summary(fe_BAI) # shows R output

### random-effects ###
re_BAI <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML")
summary(re_BAI)

### random-effects with robust variance estiamtion ###
rve_BAI <- 
  robu(
    formula = yi ~ 1, 
    var.eff.size = vi, 
    studynum = studyid,
    data = dat_BAI, 
    modelweights = "CORR",
    rho = 0.8
  )

print(rve_BAI)

sensitivity(rve_BAI)

### basic forest plot ###
forest(re_BAI)

### publication "ready" forest plot ###
re_BAI <- rma(yi = yi, vi = vi, data = dat_BAI, method = "REML",
              slab = refsummary1)
forest(re_BAI)

forest(re_BAI, order = "obs")

text(-9, 26, "Author and Year")
text(7.8, 26, "ES [95% CI]")

dat_BAI$agg_g <- aggregate(dat_BAI$yi, by = list(dat_BAI$studyid), FUN = mean)

