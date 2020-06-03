#win-stay, lose-switch analyses
#use the coupling of prev_rewarded (t-1) -> switch_choice (t) as the primary index that can be
#moderated by individual differences

library(dplyr)
library(MplusAutomation)
library(lme4)

bandit_dir<- "~/Data_Analysis/bandit_scripts"

# Node        x     y     z     Node number
# L OFC     -10    33   -20     56
# L vmPFC    -6    35   -10     84
# L rmPFC   -13    61    -4     86
# L rACC     -6    43     8     88
# L dmPFC    -9    57    20     89
# L dACC     -6    29    25     90
# R OFC      12    37   -21     159
# R rmPFC    15    63    -6     161
# R dACC      7    29    29     180
# R vmPFC     5    36   -14     191
# R rACC      8    41     5     192
# R dACC      5    28    16     193
# R dmPFC     9    56    19     194
# L BLA     -25    -5   -22     201
# R BLA      25    -3   -22     202
# L CMN     -20    -6   -15     203
# R CMN      19    -5   -15     204
# L VS      -13    12    -8     215
# R VS       11    13    -8     220


fc_data <- read.csv(file=file.path(bandit_dir, "R", "SPECC", "specc_fc_ridge_data.csv"))
fcvars <- grep("^X.*", names(fc_data), value=TRUE) #all edges

tomodel <- read.csv(file=file.path(bandit_dir, "R", "SPECC", "specc_trial_level_model_data.csv.bz2")) %>%
  mutate(BPDfac=factor(BPD, levels=c(0,1), labels=c("HC", "BPD")),
         #prev_rewarded=factor(prev_rewarded, levels=c(0,1), labels=c("no", "yes")),
         trial_z = as.vector(scale(trial)))

#start by getting basic model 'right': current shift predicted by previous characteristics
mm1 <- glmer(switch_choice ~ prev_invRT + trial_z + prev_rewarded + prev_vchosen + (1 | NUM_ID), tomodel, family=binomial())
summary(mm1)
car::Anova(mm1, type=3)
car::vif(mm1)

#allow reward x value interaction
mm2 <- glmer(switch_choice ~ prev_invRT + trial_z + prev_rewarded * prev_vchosen + (1 | NUM_ID), tomodel, family=binomial())
summary(mm2)
car::Anova(mm2, type=3)
car::vif(mm2) #some troubling collinearity between prev_rewarded and the v x reward interaction

anova(mm1, mm2)

#add abs PE
mm3 <- glmer(switch_choice ~ prev_invRT + trial_z + prev_rewarded + prev_vchosen + prev_absPE + (1 | NUM_ID), tomodel, family=binomial())
summary(mm3)
car::Anova(mm3, type=3)
car::vif(mm3)

#switch to signed PE: large positive should lead to 'stay'; large negative to 'switch'
#N.B. Model is rank deficient if we have reward, vchosen, and PE in the model
#PE should stand in for prev_rewarded in terms of its conceptual role
#this model strengthens the value effect
mm4 <- glmer(switch_choice ~ prev_invRT + trial_z + prev_vchosen + prev_PE + (1 | NUM_ID), tomodel, family=binomial())
summary(mm4)
car::Anova(mm4, type=3)
car::vif(mm4)

anova(mm1, mm2, mm3, mm4)

# a few interim conclusions:
# 1) if the previous choice was rewarded, p(switch) is much lower
# 2) if the previous choice was of high value (exploitation), p(switch) is much lower
# 3) if the previous result was associated with a large abs prediction, switch is less likely
# 4) need to sort out the equivalency of mm1==mm4 and mm2==mm3 (has to do with how value, PE, and reward play together)

#is there more switching as a function of BPD?
mmbpd <- glmer(switch_choice ~ prev_invRT + trial_z + prev_vchosen + prev_PE + BPDfac + (1 | NUM_ID), tomodel, family=binomial())
summary(mmbpd) #not in an omnibus (additive) sense

#what about interactions with value or PE?
mmbpd2 <- glmer(switch_choice ~ prev_invRT + trial_z + prev_vchosen + prev_PE * BPDfac + (1 | NUM_ID), tomodel, family=binomial())
summary(mmbpd2)
car::Anova(mmbpd2, type=3) #No

mmbpd3 <- glmer(switch_choice ~ prev_invRT + trial_z + prev_vchosen * BPDfac + prev_PE + (1 | NUM_ID), tomodel, family=binomial())
summary(mmbpd3)
car::Anova(mmbpd3, type=3) #Not here either

mmbpd4 <- glmer(switch_choice ~ prev_invRT + trial_z + prev_rewarded*BPDfac + prev_vchosen + prev_absPE + (1 | NUM_ID), tomodel, family=binomial())
summary(mmbpd4)
car::Anova(mmbpd4, type=3) #Not here either


#thus, BPD is not associated with more jumpiness or overall differences in the coupling of reward or value with win-stay lose-shift.
#but FC in selected edges could modulate win-stay lose-shift (model-free) or the coupling of value with staying.
#it's also noteworthy that the above models do not allow for random slopes (individual differences) in prev_rewarded or prev_vchosen
#these could shift whether we see BPD moderation.

mmbpd5 <- glmer(switch_choice ~ prev_invRT + trial_z + prev_rewarded*BPDfac + prev_vchosen*BPDfac + (1 + prev_vchosen + prev_rewarded | NUM_ID), tomodel, family=binomial())
summary(mmbpd5) #very sizeable variance components for the vchosen and prev_rewarded slopes (great for indiv diffs!)
car::Anova(mmbpd5, type=3)

#add brain FC to see if it relates to key effects
#drop BPD, Age, rl params in fc_data before join -- all exist in tomodel
tomodel <- tomodel %>% left_join(fc_data %>% select(NUM_ID, starts_with("X")) , by="NUM_ID")

#prototype: manually look at 161_202, which had relevance in RT models
vv <- "X161_202"
df1 <- tomodel
df1$fc <- tomodel[[vv]] #current fc

#fc variant of mm3 above -- yes, fc strongly interacts with prev_rewarded
mmfc1 <- glmer(switch_choice ~ fc + prev_invRT + trial_z + fc*prev_rewarded + fc*prev_vchosen + fc*prev_absPE + (1 | NUM_ID), df1, family=binomial())
summary(mmfc1)
car::Anova(mmfc1, type=3)


#interim conclusions:
#1) if the previous trial was rewarded, less likely to shift (i.e., win-stay)
#2) if subject chose high-value option on previous trial, less likely to shift (i.e., value-based sticking)
#3) higher FC in 161_202 is associated with weaker tendency toward win-stay (i.e., more shifting after wins)

#since this edge was chosen based on BPD vs. HC, we may have something like
#prevrew -> switch (a1) as a random slope
#then, BPD -> fc ->  a1 as mediated moderation

mm2 <- glmer(switch_choice ~ fc + prev_invRT + trial_z + fc*prev_rewarded*BPDfac + prev_vchosen*fc + (1 + prev_rewarded | NUM_ID), df1, family=binomial())
summary(mm2)
car::Anova(mm2, type=3)

#move toward Mplus MSEM because it alows us to test for BPD -> FC -> trial mediated moderation (prevrew -> stay) effects
#because FC edges were selected on the basis of discriminating HC/BPD, it seems like a possible double dipping problem if we start using
#them in other ways that suggest independence. Although the groups don't differ on *all* edges in terms of mean level, they differ on most,
#which is a function of the feature selection.

#thus, we have 'baked in' edges that have BPD -> FC differences and now we want to see whether there are FC -> behavior effects.
#If each part of the chain holds, then we move forward to BPD -> FC -> behavior. This is only (easily) possible in MSEM since we
#want the indirect effects on the between level.

#base MSEM object
mobj <- mplusObject(
  TITLE="Win-stay lose-switch as a function of FC",
  ANALYSIS="TYPE=TWOLEVEL RANDOM;
    ESTIMATOR=BAYES; 
    FBITERATIONS=20000;
    PROCESSORS = 4;
  ",
  VARIABLE = "
    CATEGORICAL=switch_choice;
    BETWEEN=fc;
    CLUSTER=NUM_ID;
    WITHIN=trial_z prev_invRT; !addl covariates
  ",
  MODEL="
  %WITHIN%
    stay_slo | switch_choice ON prev_rewarded;
    switch_choice ON prev_vchosen prev_invRT trial_z;
  %BETWEEN%
    stay_slo ON fc;
    prev_vchosen prev_rewarded; !overall choice quality variables
    switch_choice WITH stay_slo;
  ",
  rdata = tomodel,
  usevariables = c("switch_choice", "prev_rewarded", "prev_vchosen", "prev_invRT", "trial_z", "NUM_ID", "fc"),
  OUTPUT="STANDARDIZED CINTERVAL TECH8;"
)


mplus_params <- list()
mplus_results <- list()
mplus_params_bpd <- list()
mplus_results_bpd <- list()

for (vv in fcvars) {
  df1 <- tomodel
  df1$fc <- tomodel[[vv]] #current fc
  this <- update(mobj, TITLE = ~ . + paste0("Connection:" + vv),
                 rdata=df1)
  
  if (!file.exists(paste0("msem_fc", vv, ".out"))) {
    mres <- mplusModeler(this, modelout=paste0("msem_fc", vv, ".inp"), run=TRUE, hashfilename = FALSE,
                         Mplus_command = "/Users/mnh5174/Applications/Mplus/mplus")$results
  } else {
    mres <- readModels(paste0("msem_fc", vv, ".out"))
  }
  
  mplus_params[[vv]] <- mres$parameters
  mplus_results[[vv]] <- mres
  
  #add BPD
  this <- update(
    mobj, TITLE = ~ . + paste0("Connection:", vv, ", BPD mediation"),
    usevariables=c(mobj$usevariables, "BPD"),
    VARIABLE= ~"
      CATEGORICAL=switch_choice;
      BETWEEN=fc BPD;
      CLUSTER=NUM_ID;
      WITHIN=trial_z prev_invRT;",
    MODEL= ~"
      %WITHIN%
  
        stay_slo | switch_choice ON prev_rewarded;
        switch_choice ON prev_vchosen prev_invRT trial_z;
      %BETWEEN%
        stay_slo ON fc (b)
          BPD (cprime);
        fc ON BPD (a);
        prev_vchosen prev_rewarded; !overall choice quality variables
        switch_choice WITH stay_slo;",
    MODELCONSTRAINT= ~ "NEW (ind); ind=a*b;",
    rdata=df1
  )
  
  if (!file.exists(paste0("msem_fc_bpd", vv, ".out"))) {
    mres_bpd <- mplusModeler(this, modelout=paste0("msem_fc_bpd", vv, ".inp"), run=TRUE, hashfilename = FALSE,
                             Mplus_command = "/Users/mnh5174/Applications/Mplus/mplus")$results
  } else {
    mres_bpd <- readModels(paste0("msem_fc_bpd", vv, ".out"))
  }
  mplus_params_bpd[[vv]] <- mres_bpd$parameters
  mplus_results_bpd[[vv]] <- mres_bpd
  
}

#combine and examine
ind_effects <- lapply(1:length(mplus_params_bpd), function(m) {
  pars <- mplus_params_bpd[[m]]$unstandardized
  pars$fc <- names(mplus_params)[m]
  pars <- pars %>% filter(paramHeader %in% c("New.Additional.Parameters", "STAY_SLO.ON", "FC.ON"))
  return(pars)
})

ind_effects <- bind_rows(ind_effects) 
sigind <- ind_effects %>% filter(param=="IND" & pval < .06) %>% pull(fc)
ind_effects_sig <- ind_effects %>% filter(fc %in% sigind) %>% arrange(fc, paramHeader)
ind_effects_sig %>% split(.$fc)

#the pattern here is generally consistent except for 161_202: 
# 1) The stay_slo mean is negative -- meaning that previously rewarded trials are associated with lower p(switch)
# 2) All of these edges are associated with more positive prev_reward -> switch slopes. In other words, strong FC for
#       these edges is associated with *weaker* win stay behavior after a reward.
# 3) 5 of 6 edges show lower FC in BPD than HC, meaning that they support stronger win-stay behaviors in BPD than in HC
# 4) 1 of 6 (161_202) shows the opposite pattern: stronger FC in BPD supports more win-switch behaviors (i.e., more positive slope)

save.image(file="winstay_analyses.RData")