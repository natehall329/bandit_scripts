library(lme4)
library(emmeans)
library(sjPlot)
library(tidyverse)

bandit_dir<- "~/Data_Analysis/bandit_scripts"
#mount_dir <- "/Users/Shared/ics"
#mount_dir <- "/Users/michael/ics"
#basedir <- paste0(mount_dir,"/Michael/bpd_rest")

fc_data <- read.csv(file=file.path(bandit_dir, "R", "SPECC", "specc_fc_ridge_data.csv"))
fcvars <- grep("^X.*", names(fc_data), value=TRUE) #all edges

#are there big group diffs in FC in these edges? there should be a lot since we're selecting on logit(BPD ~ FC)
for(vv in fcvars) {
  cat("var: ", vv, "\n")
  print(t.test(as.formula(paste(vv, "~BPD_fac")), fc_data))
}


tomodel <- read.csv(file=file.path(bandit_dir, "R", "SPECC", "specc_trial_level_model_data.csv.bz2")) %>%
  mutate(BPDfac=factor(BPD, levels=c(0,1), labels=c("HC", "BPD")),
#         prev_rewarded=factor(prev_rewarded, levels=c(0,1), labels=c("no", "yes")),
         trial_z = as.vector(scale(trial)))

m1 <- lmer(invRT ~ prev_invRT + prev_rewarded * BPDfac + (1 | NUM_ID) , tomodel)
summary(m1)
car::Anova(m1, type=3)
pairs(emtrends(m1, var="prev_rewarded", specs =~BPDfac))
plot(emtrends(m1, var="prev_rewarded", specs =~BPDfac))


m2 <- lmer(invRT ~ prev_rewarded * BPQ_total + abspe * BPQ_total + trial + (1 | NUM_ID) , tomodel)
summary(m2)
car::Anova(m2, type=3)
plot_model(m2, type="pred", terms=c("BPQ_total", "prev_rewarded")) #more of a slowdown for high BPD folks


m2 <- lmer(invRT ~ prev_rewarded * BPQ_total + abspe * BPQ_total + trial + (1 | NUM_ID) , tomodel)
summary(m2)
car::Anova(m2, type=3)
library(sjPlot)
plot_model(m2, type="pred", terms=c("BPQ_total", "prev_rewarded")) #more of a slowdown for high BPD folks


m3 <- lmer(invRT ~ prev_invRT + prev_rewarded * BPQ_total + abspe * BPQ_total + trial + (1 | NUM_ID) , tomodel)
summary(m3)
car::Anova(m3, type=3)
plot_model(m3, type="pred", terms=c("BPQ_total", "prev_rewarded")) #more of a slowdown for high BPD folks

#mimic dombrovski 2018 model
#looks like it was 1000/rt, not -1000/rt -- need to invert all coefficients mentally?
m4 <- lmer(invRT ~ prev_invRT + trial_z + switch_choice * prev_rewarded +
             prev_rewarded + prev_vdiff + prev_absPE +
             prev_vmax + 
             BPDfac*prev_rewarded + BPDfac*prev_absPE + BPDfac*prev_vmax +
             (1 + trial_z| NUM_ID) , tomodel)


summary(m4)
car::Anova(m4, type=3)
car::vif(m4)
plot_model(m4, type="pred", terms=c("BPDfac", "prev_rewarded")) #more of a slowdown for high BPD folks

#we see that if the previous trial was not rewarded, the 
#BPD group shows a 'screw it' attitude and has faster responses on the next trial
#this manifests as an apparently greater slowing post-reward, but the plot_model shows that it's more
#of the 'screw it'
emtrends(m4, var="prev_rewarded", specs =~BPDfac)
#pairs(emmeans(m4, specs =~prev_rewarded * BPDfac)) #if we code rewarded as a factor

#after a large abs PE on t-1, the BPD group shows greater slowing
emtrends(m4, var="prev_absPE", specs =~BPDfac)
pairs(emtrends(m4, var="prev_absPE", specs =~BPDfac))

#simplify to look at vchosen
m4 <- lmer(invRT ~ prev_invRT + trial_z + switch_choice * prev_rewarded +
             prev_rewarded + prev_vdiff + prev_absPE +
             prev_vmax + 
             BPDfac*prev_rewarded + BPDfac*prev_absPE + BPDfac*prev_vmax +
             (1 + trial_z| NUM_ID) , tomodel)


#-----
#add brain FC to see if it relates to key effects
#drop BPD, Age, rl params in fc_data before join -- all exist in tomodel
tomodel <- tomodel %>% left_join(fc_data %>% select(NUM_ID, starts_with("X")) , by="NUM_ID")

allres <- c()
mlist <- list()
for (vv in fcvars) {
  df1 <- tomodel
  df1$fc <- tomodel[[vv]] #current fc
  mm <- lmer(invRT ~ prev_invRT + trial_z + switch_choice * prev_rewarded +
             prev_rewarded + prev_vdiff + prev_absPE +
             prev_vdiff*fc*BPDfac +
             BPDfac*prev_rewarded + BPDfac*prev_absPE + BPDfac*prev_vmax + (1 | NUM_ID), df1)
  #str(car::Anova(mm, type=3))
  #res <- tidy(mm)
  res <- car::Anova(mm, type=3) %>% mutate(eff=row.names(.)) %>% filter(grepl("fc", eff)) %>% mutate(fc=vv) %>%
    dplyr::rename(pval=`Pr(>Chisq)`) %>% select(fc, eff, everything())
  allres <- rbind(allres, res)
  mlist[[vv]] <- mm
}

#screen results
allres %>% filter(pval < .05)

#look at 88_99, which has largest chi-square, as an example
thism <- mlist[["X88_99"]]
summary(thism)
emtrends(thism, var="prev_vdiff", specs=~BPDfac)

#need to get quantiles of FC for this to make sense because it's 3-way
qfc <- quantile(fc_data$X88_99, c(.25, .5, .75)) %>% round(3)
qvc <- quantile(tomodel$prev_vdiff, c(.25, .5, .75), na.rm=T) %>% round(3)
# predlist <- list(fc=qfc, prev_vdiff=qvc)
# emtrends(thism, var="prev_vdiff", specs=~1, at=predlist)

emmip(thism, prev_vdiff~fc | BPDfac, at=predlist, CIs=TRUE)

library(reghelper)
#get simple slopes of prev_vdiff across FC values in the two groups
predlist <- list(fc=qfc, prev_vdiff="sstest", BPDfac=c("HC", "BPD"))
simple_slopes(thism, levels=predlist)

#in controls, greater FC is associated with decreasing coupling of prev value diff (Vchoice) with RT
#while in BPD, fc scales positively with Vchoice -> RT coupling. Need to remind myself of the direction here
# a positive prev_vdiff coefficient means that if the subject did not choose the best option (Vmax) on the previous
# trial (i.e., a negative Vchoice/prev_vdiff), then they sped up more on the current trial +Beta * -vdiff.
# so, it looks like after an exploratory choice, X88_99 supports slowing down in controls, but speeding up in BPD

plot_model(thism, type = "pred", terms=c("prev_vdiff", "fc", "BPDfac"))
lattice::histogram(~X88_99|BPD_fac, fc_data) #it is one where there's a group difference, but also a lot of overlap
t.test(X88_99~BPD_fac, fc_data)


## look at FC relevance to the task before we build up a complicated model


allres <- c()
mlist <- list()
for (vv in fcvars) {
  df1 <- tomodel
  df1$fc <- tomodel[[vv]] #current fc
  mm <- lmer(invRT ~ prev_invRT + trial_z + prev_rewarded +
              prev_absPE*fc + prev_vchosen*fc*prev_absPE + (1 | NUM_ID), df1)
  #str(car::Anova(mm, type=3))
  #res <- tidy(mm)
  res <- car::Anova(mm, type=3) %>% mutate(eff=row.names(.)) %>% filter(grepl("fc", eff)) %>% mutate(fc=vv) %>%
    dplyr::rename(pval=`Pr(>Chisq)`) %>% select(fc, eff, everything())
  allres <- rbind(allres, res)
  mlist[[vv]] <- mm
}

#screen results
allres %>% filter(pval < .05)

#how about 161_202 as an example?
thism <- mlist[["X161_202"]]
summary(thism)
emtrends(thism, var="prev_vdiff", specs=~BPDfac)

qfc <- quantile(fc_data$X161_202, c(.25, .5, .75)) %>% round(3)
qvc <- quantile(tomodel$prev_vchosen, c(.25, .5, .75), na.rm=T) %>% round(3)

predlist <- list(fc=qfc, prev_vchosen=c(qvc, "sstest"))
simple_slopes(thism, levels=predlist)

#manually look at 161_202
#NB. We should probably be using prev_switch_choice in general since at the moemnt, switch choice models the impact of
#a shift that is about to happen on that shift's RT... this is interesting (basically a cognitive lag), but is
#different in spirit.
vv <- "X161_202"
df1$fc <- tomodel[[vv]] #current fc
mm <- lmer(invRT ~ prev_invRT*fc + trial_z + prev_rewarded + switch_choice +
             prev_vchosen*fc + (1 | NUM_ID), df1)
summary(mm)
car::Anova(mm, type=3)

