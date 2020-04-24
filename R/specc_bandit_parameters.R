#examine parameters from bandit RS
library(tidyverse)
library(GGally)
library(tidylog)
library(skimr)
pars <- read.csv("/Users/michael/Data_Analysis/bandit_scripts/vba_output/specc_twolr_decay_mfx_bandit_global_statistics.csv")
trans <- pars %>% select(ends_with("transformed"), "R2")
ggpairs(trans)

#one subject has an obscene beta relative to the group.
#this subject also has a terrible R2
pars %>% filter(beta_transformed > .5)

trans_filt <- pars %>% select(ends_with("transformed"), "R2") %>% filter(beta_transformed < .5)
ggpairs(trans_filt)

skim(trans_filt)

pars %>% filter(beta_transformed > .35)

#look at ffx parameters as a guide for whether some subjects are even worse when they're not pulled toward the group
pars %>% arrange(beta_transformed_ffx) %>% tail()
hist(pars$beta_transformed_ffx)

#poor fit
pars %>% arrange(R2) %>% head()
hist(pars$R2)


#subject 73 is definitely dead in the water and should not be considered, or even fit in the first place
#I have now gone back and refitted MFX without this person
