################### Effect Size Calculations #########################

#### Load packages ####
library(metafor)
library(tidyverse)
library(readxl)

setwd("/home/saurabh/Everything/PhD/Workshops/Meta Analysis Course/my_analysis")

## join checks ##

read_xlsx("L&L Data Set Means SDs.xlsx", sheet = "VS") %>% 
  select(AUTYR) %>% 
  drop_na(AUTYR) %>% 
  anti_join(
    read_xlsx("L&L Data Set Means SDs.xlsx", sheet = "StudyChar") %>% 
      select(AUTYR) %>% 
      drop_na(AUTYR),
    by = "AUTYR"
  )






#### Load dataset ####
df <- read_xlsx("L&L Data Set Means SDs.xlsx", sheet = "VS")
 
#### Calculate effect sizes using metafor ####

# drop completely NA columns  
df <- 
  df %>% 
  select_if(~ any(!is.na(.)))

# post test only studies
df <-
  df %>% 
  filter(is.na(T1MVR1pre)) %>%
  select_if(~ any(!is.na(.)))

df <-
  escalc(
    measure = "SMD", # Tells R to calculate standardized mean-difference (Hedges' g)
    m1i = T1MVR1post, m2i = CMVR1post, # Treatment and control means
    sd1i = T1SVR1post, sd2i = CSVR1post, # Treatment and control SDs
    n1i = T1NVR1post, n2i = CNVR1post, # Treatment and control sample size
    data = df, # Dataset
    append = TRUE
  ) # Tell R to put ES and var in current dataset
 
df %>% view()
