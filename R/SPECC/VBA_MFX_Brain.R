# inspect VBA MFX output and look for associations with connectivity metrics. --------------------------------------------------

pacman::p_load(R.matlab, dependlab, sigmoid, beepr, psych, MuMIn, ggcorrplot, mediation, furrr, broom, tidyverse, GGally, cowplot, tidylog)

# bandit_dir<- "/Users/nth7/Desktop/bandit_scripts"
# mount_dir <- "/mnt/ics"

bandit_dir<- "~/Data_Analysis/bandit_scripts"
#mount_dir <- "/Users/Shared/ics"
mount_dir <- "/Users/michael/ics"

params <- read.csv("~/Data_Analysis/bandit_scripts/vba_output/specc_twolr_decay_mfx_bandit_global_statistics.csv")
params <- params %>% dplyr::rename(NUM_ID=id)

## these are most of the requisite dfs from the resting state analysis.
basedir <- paste0(mount_dir,"/Michael/bpd_rest")
load(paste0(basedir, "/output.files/results_005_200.RData"))  #results_005. results of the ridge regression that fell below a p-value of .01. Though not all subnuclei of the amygdala were signifcant at this step, we included all in the GIMME analysis.
scaled <- get(load(paste0(basedir, "/cache/scaled_inputs_all_200.RData"))) #scaled. large list with fl and soc dfs to be input to logistic ridge
load(file.path(basedir, "cache/subjinfo_schaefer421_fsl_prewhitened_pearson.RData")) #subj_info
# load(file.path(basedir, "behavior/sr_behav_outcomes.RData")) #contains BPQ, DERS, emo-gender-conflict,

rl_subs <- subj_info %>% left_join(params, by = "NUM_ID") %>% data.frame() %>%
  dplyr::select(contains("ID"), BPD, AgeAtScan, alpha_win, alpha_loss, decay, beta) %>% # combine relevant subject info with params from RL model
  mutate(BPD_fac=factor(BPD, levels = c(0,1), labels = c("HC", "BPD")))

rl_subs %>% filter(is.na(alpha_win)) # who is missing bandit fits?

#scaled seems to lack an id columns -- be damned sure we've got the order right before a join
stopifnot((cor(rl_subs$AgeAtScan, scaled$fl$Age) - 1.0) < 1e-5)
apply(cbind(scale(rl_subs$AgeAtScan), scaled$fl$Age), 1, function(r) {r[1]==r[2]})

#ggcorrplot(rl_subs %>% select(-contains("ID")) %>% cor(use="pairwise"))

#these are the corrected error files after fixing eprimeread
b_files <- list.files(path="~/Data_Analysis/bandit_scripts/vba_output/b_outputs", pattern="\\.csv\\.bz2", full.names = TRUE)
b_outputs_all <- bind_rows(lapply(b_files, read_csv)) %>% group_by(id) %>% mutate(trial=1:n()) %>% ungroup() %>%
  dplyr::rename(NUM_ID=id, rewarded=stim_ACC)

#add win-stay, lose-shift and other more straightforward model-free statistics
b_outputs_all <- b_outputs_all %>% group_by(NUM_ID) %>%
  mutate(prev_choice=dplyr::lag(stim_choice),  #outputs should already be in trial order
         prev_rewarded=dplyr::lag(rewarded),
         switch_choice=as.numeric(stim_choice!=prev_choice),
         lose_switch=as.numeric(switch_choice & !prev_rewarded),
         win_switch=as.numeric(switch_choice & prev_rewarded),
  ) %>% ungroup()

b_outputs_all %>% select(rewarded, stim_choice, prev_choice, switch_choice, prev_rewarded, win_switch, lose_switch) %>% head(n=100) #looks right

b_outputs_summary <- b_outputs_all %>% group_by(NUM_ID) %>%
  dplyr::summarize(perseverative = sum(perseverative),
                   prob_switch_err = sum(prob_switch_err),
                   spont_switch_err = sum(spont_switch_err),
                   erratic_spont = sum(erratic_spont),
                   error_NOS = sum(error_NOS),
                   explore_switch = sum(explore_switch),
                   lose_switch = sum(lose_switch, na.rm=TRUE),
                   win_switch = sum(win_switch, na.rm=TRUE)) %>%
  mutate(perseverative_log=log(perseverative))

hist(b_outputs_summary$win_switch)
hist(b_outputs_summary$lose_switch)

#for a mediation model to work out, the X -> M relationship needs to hold
#screen the RL parameters and model-free statistics for associations with BPD and impulsivity
bpq <- read.csv("~/Data_Analysis/SPECC/self_reports/data/surveys/baseline/scored/BPQ_scored.csv") %>%
  dplyr::select("SPECC_ID", "BPQ_impuls", "BPQ_instab", "BPQ_abandon", "BPQ_relations", "BPQ_self", "BPQ_suicide",
                "BPQ_empti", "BPQ_anger", "BPQ_psycho", "BPQ_total") %>%
  dplyr::mutate(SPECC_ID = sub("_", "", SPECC_ID))

#combined data
upps <- read.csv("~/Data_Analysis/SPECC/self_reports/data/SPECC_UPPS_all_28Apr2020.csv") %>%
  dplyr::select(SPECC_ID, ACID, VisitMonth, UPPS_negurg, UPPS_lackprem, UPPS_lackpers,	UPPS_senseek, UPPS_posurg) %>%
  dplyr::mutate(SPECC_ID = sub("_", "", SPECC_ID)) %>% filter(VisitMonth==0) %>% select(-VisitMonth)

#this is now handled in UPPS_Checks.R
#looks like there is a dupe in UPPS for 040BS... This person has two records: 6/12/14 and 8/11/14.
#the latter date corresponds to the record in the SPECC participation flow. And it's more recent, so use it
#upps$SPECC_ID[duplicated(upps$SPECC_ID, incomparables = NA)]
#upps <- upps %>% filter(ACID != 36420)

combined <- b_outputs_summary %>% left_join(subj_info %>% dplyr::select(NUM_ID, SPECC_ID, BPD, AgeAtScan, Female)) %>%
  left_join(upps) %>% left_join(bpq) %>% left_join(params %>% dplyr::select(-dataset, -model, -evo_fname, -obs_fname))

#screen for correlations between clinical scales and RL parameters: connection between UPPS sensation seeking and alpha_win
cor_with_target(combined %>% as.data.frame(),
                target = c("F", "LL", "AIC", "BIC", "R2", "alpha_win", "alpha_loss", "decay", "beta",
                           "alpha_win_transformed", "alpha_loss_transformed", "decay_transformed",
                           "beta_transformed", "alpha_win_ffx", "alpha_loss_ffx", "decay_ffx",
                           "beta_ffx", "alpha_win_transformed_ffx", "alpha_loss_transformed_ffx",
                           "decay_transformed_ffx", "beta_transformed_ffx"),
                withvars = c("BPD", "AgeAtScan", "Female", "UPPS_negurg", "UPPS_lackprem",
                             "UPPS_lackpers", "UPPS_senseek", "UPPS_posurg", "BPQ_impuls",
                             "BPQ_instab", "BPQ_abandon", "BPQ_relations", "BPQ_self", "BPQ_suicide",
                             "BPQ_empti", "BPQ_anger", "BPQ_psycho", "BPQ_total"), pmin=.1, orderbyr = TRUE)

#model-free indices: not much!
cor_with_target(combined %>% as.data.frame(),
                target = c("perseverative", "prob_switch_err", "spont_switch_err",
                           "erratic_spont", "error_NOS", "explore_switch", "perseverative_log", "lose_switch", "win_switch"),
                withvars = c("BPD", "AgeAtScan", "Female", "UPPS_negurg", "UPPS_lackprem",
                             "UPPS_lackpers", "UPPS_senseek", "UPPS_posurg", "BPQ_impuls",
                             "BPQ_instab", "BPQ_abandon", "BPQ_relations", "BPQ_self", "BPQ_suicide",
                             "BPQ_empti", "BPQ_anger", "BPQ_psycho", "BPQ_total"), pmin = .1, orderbyr = TRUE)




#basic trial-level modeling of RTs
bjoin <- b_outputs_all %>% left_join(subj_info %>% dplyr::select(NUM_ID, SPECC_ID, BPD, AgeAtScan, Female)) %>%
  left_join(upps) %>% left_join(bpq) %>% left_join(params %>% dplyr::select(-dataset, -model, -evo_fname, -obs_fname)) %>%
  mutate(invRT=-1000/stim_RT) %>% filter(stim_RT > 125 & stim_RT < 5000) #sane values

#NB: Looks like PEchosen is already shifted (lead by 1) to align on the current trial. So if we want it to be PE(t-1), that would require lagging again
#but PE (the hidden state) is t-1
trial_stats <- read.csv("~/Data_Analysis/bandit_scripts/vba_output/specc_twolr_decay_mfx_bandit_trial_outputs.csv.bz2") %>%
  dplyr::rename(trial=obs_number, NUM_ID=id)

#the additional value indices added to the trial_outputs are peculiar in terms of their alignment.
#just recompute here to keep myself sane
tomodel <- trial_stats %>% left_join(bjoin, by=c("NUM_ID", "trial")) %>% mutate(abspe=abs(PEchosen)) %>%
  group_by(NUM_ID) %>%
  dplyr::rename(prev_PE = PE) %>% #hidden state for PE is always t-1 in VBA
  mutate(
    prev_invRT = dplyr::lag(invRT, order_by=trial)
  ) %>% ungroup()

#pA is 1/0 indicating whether A was chosen -- hence rowSums of the multiplication will filter value chosen
tomodel$vchosen <- rowSums(tomodel[,c("pA", "pB", "pC")] * tomodel[,c("QA", "QB", "QC")])
tomodel$vmax <- apply(tomodel[,c("QA", "QB", "QC")], 1, max, na.rm=TRUE)
tomodel$vdiff <- tomodel$vchosen - tomodel$vmax #0 if best option chosen, negative if suboptimal chosen

tomodel <- tomodel %>%
  group_by(NUM_ID) %>%
  mutate(
    prev_vmax=dplyr::lag(vmax, order_by=trial),
    prev_vchosen=dplyr::lag(vchosen, order_by=trial),
    prev_vdiff=dplyr::lag(vdiff, order_by=trial),
    prev_absPE=abs(prev_PE)
  ) %>% ungroup() %>%
  select(NUM_ID, trial,
         pA, pB, pC, QA, QB, QC,
         invRT, prev_invRT, switch_choice, rewarded, prev_rewarded, 
         vmax, prev_vmax, vchosen, prev_vchosen, 
         vdiff, prev_vdiff, prev_PE, prev_absPE,
         starts_with("UPPS"), starts_with("BPQ"), BPD, AgeAtScan, Female, 
         alpha_win, alpha_loss, beta, decay, R2)

write.csv(tomodel, file=file.path(bandit_dir, "R", "specc_trial_level_model_data.csv"), row.names = FALSE)


### BRING IN DA BRAIN

## pulled from another script. This just grabs all significant (from "feature selection" ridge regresssion step) FC predictors of interest into one big df.
if(file.exists(paste0(basedir,"/cache/neural_mediators.RData"))){
  load(paste0(basedir,"/cache/neural_mediators.RData"))
} else {

  neural_mediators <- do.call(cbind,lapply(results_005, function(x){
    y <- unique(x$rowname)
    names <- gsub("_l", "", y)
    out <- list()
    for(i in names) {
      for(j in scaled) {
        effects <- j[,which(grepl(gsub("`", "",i),colnames(j)))]
        effects <- effects[,which(!grepl("_l", colnames(effects)))]
        if(length(effects != 0)){
          out[[i]] <- effects
        }
      }
    }
    out <- do.call(cbind, out)

    out
  }))

  #fix weirdness with 90-215, dont want to fix more prettily
  neural_mediators <- neural_mediators[,-which(colnames(neural_mediators) == "`90_215`.90_215_i")]
  colnames(neural_mediators)[which(colnames(neural_mediators) == "`90_215`.90_215")] <- "`90_215`"
  colnames(neural_mediators) <- gsub("`", "", colnames(neural_mediators))
  neural_mediators$Age <- subj_info$AgeAtScan
  save(neural_mediators, file = paste0(basedir,"/cache/neural_mediators.RData"))

}

#verify matching between neural mediators and RL
stopifnot((cor(rl_subs$AgeAtScan, neural_mediators$Age) - 1.0) < 1e-5)
apply(cbind(scale(rl_subs$AgeAtScan), scale(neural_mediators$Age)), 1, function(r) {r[1]==r[2]})

rl_all <- cbind(rl_subs,neural_mediators %>% select(-Age) )

write.csv(rl_all, file=file.path(bandit_dir, "R", "specc_fc_ridge_data.csv"), row.names = FALSE)
####

toplot <- reshape2::melt(rl_subs %>% select(-contains("ID"), -BPD_fac), id.vars = c("AgeAtScan", "BPD"))
toplot$BPD <- factor(toplot$BPD, levels = c(0,1), labels = c("HC", "BPD"))

ggplot(toplot, aes(x = AgeAtScan, y= value, color = BPD)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~variable, scales = "free_y", ncol = 4)

#all weak effects, though some suggestion from the plots
m1 <- lm(alpha_win ~ AgeAtScan*BPD, rl_subs)

m2 <- lm(alpha_loss ~ AgeAtScan*BPD, rl_subs)
summary(m2)
# library(MASS)
# m2 <- rlm(alpha_loss ~ AgeAtScan*BPD, rl_subs)

m3 <- lm(decay ~ AgeAtScan*BPD, rl_subs)
summary(m3)

m4 <- lm(beta ~ AgeAtScan*BPD, rl_subs)
summary(m4)

m4 <- lm(beta ~ AgeAtScan*BPD, rl_subs)
summary(m4)

m4 <- rlm(beta ~ AgeAtScan*BPD, rl_subs)
summary(m4)

g1 <- ggpairs(rl_all[,6:12])
g1

pdf("rl_scatter.pdf", width=10, height=10)
numcols <- paste0("`", grep("\\d+_\\d+", names(rl_all), perl=TRUE, value=TRUE), "`")
for (n in numcols) {
  g1 <- ggplot(rl_all, aes_string(x="alpha_win", y=n)) + geom_point() + geom_smooth(method="lm") + ggtitle(paste0("alpha_win with ", n))
  g2 <- g1 + aes_string(x="alpha_loss") + ggtitle(paste0("alpha_loss with ", n))
  g3 <- g1 + aes_string(x="decay") + ggtitle(paste0("decay with ", n))
  g4 <- g1 + aes_string(x="beta") + ggtitle(paste0("beta with ", n))

  plot(plot_grid(g1, g2, g3, g4, nrow=2))
}
dev.off()

#there are 22 FC values and 4 RL parameters
numcols <- grep("\\d+_\\d+", names(rl_all), perl=TRUE, value=TRUE)
cor_sum <- rl_all %>% pivot_longer(cols=all_of(numcols), names_to="conn_name", values_to="fconn") %>%
  pivot_longer(cols=alpha_win:beta, names_to="rl_name", values_to="rl_par")

cor_mat <- cor_sum %>% group_by(rl_name, conn_name) %>%
  summarize(rcorr=cor(fconn, rl_par, use="pairwise")) %>% ungroup()

pdf("cor_heat.pdf", width=15, height=5)
ggplot(cor_mat, aes(x=conn_name, y=rl_name, fill=rcorr)) + geom_tile() + scale_fill_viridis_c()
dev.off()

#with age
dmat <- cor_sum %>% nest(data=c(-rl_name, -conn_name)) %>%
  mutate(mres = map(data, ~ lm(fconn ~ rl_par*BPD*AgeAtScan, .)),
         tidy_res=map(mres, tidy)) %>%
  unnest(tidy_res)

#dmat %>% filter(term != "(Intercept)" & p.value < .05 & grepl("rl_par", term)) %>% View()

#without age
dmat <- cor_sum %>% nest(data=c(-rl_name, -conn_name)) %>%
  mutate(mres = map(data, ~ lm(fconn ~ rl_par*BPD, .)),
         tidy_res=map(mres, tidy)) %>%
  unnest(tidy_res)

dmat %>% filter(term != "(Intercept)" & p.value < .1 & grepl("rl_par", term))

#mediation
plan(multiprocess) #enable multi-processor computation using future_map
cor_sum <- cor_sum %>% mutate(BPDfac=factor(BPD, levels=c(0,1), labels=c("HC", "BPD")))
dmat <- cor_sum %>% nest(data=c(-rl_name, -conn_name)) %>%
  mutate(mb = map(data, ~ lm(rl_par ~ BPD, .)),
         mc = map(data, ~ lm(fconn ~ BPD + rl_par, .)),
         #mmed = map2(mb, mc, ~ mediate(model.m=.x, model.y=.y, treat="BPD", mediator="rl_par", robustSE=TRUE, use_speed=TRUE)), #sims=100, boot=TRUE,
         mmed = future_map2(mb, mc, ~ mediation::mediate(
           model.m=.x, model.y=.y, treat="BPD", mediator="rl_par", control.value=0, treat.value=1,
           robustSE=TRUE, use_speed=TRUE)), #sims=100, boot=TRUE,
         #tidy_mb=map(mb, tidy),
         tidy_med=map(mmed, tidy)
  )

#get just the indirect effect -- nothing going on!
xx <- dmat %>% mutate(tidy_med=map(mmed, tidy)) %>% unnest(tidy_med) %>% dplyr::select(-data, -mb, -mc, -mmed) %>%
  filter(grepl("acme_1", term))

View(xx)

#what about age
plan(multiprocess) #enable multi-processor computation using future_map
cor_sum <- cor_sum %>% mutate(BPDfac=factor(BPD, levels=c(0,1), labels=c("HC", "BPD")))
dmat <- cor_sum %>% nest(data=c(-rl_name, -conn_name)) %>%
  mutate(mb = map(data, ~ lm(rl_par ~ BPDfac + AgeAtScan, .)),
         mc = map(data, ~ lm(fconn ~ BPDfac + rl_par + AgeAtScan, .)),
         #mmed = map2(mb, mc, ~ mediate(model.m=.x, model.y=.y, treat="BPD", mediator="rl_par", robustSE=TRUE, use_speed=TRUE)), #sims=100, boot=TRUE,
         mmed = future_map2(mb, mc, ~ mediation::mediate(
           model.m=.x, model.y=.y, treat="AgeAtScan", mediator="rl_par", covariates="BPDfac", control.value=13, treat.value=26,
           robustSE=TRUE, use_speed=TRUE)), #sims=100, boot=TRUE,
         #tidy_mb=map(mb, tidy),
         tidy_med=map(mmed, tidy)
  )

summary(dmat$mmed[[1]]) #check on one model

#no age -> rl -> fconn relationship
xx <- dmat %>% mutate(tidy_med=map(mmed, tidy)) %>% unnest(tidy_med) %>% dplyr::select(-data, -mb, -mc, -mmed) %>%
  filter(grepl("acme_1", term) & p.value < .1)

cor_sum <- cor_sum %>% left_join(upps)

dmat <- cor_sum %>% nest(data=c(-rl_name, -conn_name)) %>%
  mutate(mb = map(data, ~ lm(rl_par ~ UPPS_negurg, .)),
         mc = map(data, ~ lm(fconn ~ rl_par + UPPS_negurg, .)),
         mmed = future_map2(mb, mc, ~ mediation::mediate(
           model.m=.x, model.y=.y, treat="UPPS_negurg", mediator="rl_par", control.value=1, treat.value=3,
           robustSE=TRUE, use_speed=TRUE)), #sims=100, boot=TRUE,
         #tidy_mb=map(mb, tidy),
         tidy_med=map(mmed, tidy)
  )

#RL -> fconn relationships are all ~0, so mediation isn't really possible without an m->y relationship.
xx <- dmat %>% mutate(tidy_med=map(mmed, tidy)) %>% unnest(tidy_med) %>% dplyr::select(-data, -mb, -mc, -mmed) %>%
  filter(grepl("acme_1", term))# & p.value < .1)


#validations
#single model
mb <- lm(rl_par ~ BPDfac, dmat$data[[1]])
mc <- lm(fconn ~ BPDfac + rl_par, dmat$data[[1]])

xx <- mediation::mediate(
  model.m=mb, model.y=mc, treat="BPDfac", mediator="rl_par", control.value="HC", treat.value="BPD",
  robustSE=TRUE, use_speed=TRUE, boot=1000)



xx <- mediation::mediate(
  model.m=dmat$mb[[1]], model.y=dmat$mc[[1]], treat="BPD", mediator="rl_par", control.value=0, treat.value=1,
  robustSE=TRUE, use_speed=TRUE, boot=1000)

summary(xx)
xx$d0
xx$d1

xx <- dmat %>% mutate(tidy_med=map(mmed, tidy)) %>% unnest(tidy_med) %>% dplyr::select(-data, -mb, -mc, -mmed) %>%
  filter(grepl("acme_1", term))


b <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
c <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)

# Estimation via quasi-Bayesian approximation
contcont <- mediation::mediate(b, c, sims=50, treat="treat", mediator="job_seek", robustSE = TRUE, use_speed = TRUE)
tidy(contcont)



b <- lm(job_seek ~ educ + sex, data=jobs)
c <- lm(depress2 ~ educ + job_seek + sex, data=jobs)

# compare two categories of educ --- gradwk and somcol
model.cat <- mediate(b, c, treat="educ", mediator="job_seek", sims=50,
                     control.value = "gradwk", treat.value = "somcol")
summary(model.cat)


##### model-free behavioral indices

hist(sqrt(b_outputs_summary$prob_switch_err))

fconn_long <- rl_all %>% pivot_longer(cols=numcols, names_to="conn_name", values_to="fconn")

fconn_long <- fconn_long %>% left_join(b_outputs_summary)


dmat <- fconn_long %>% nest(data=c(-conn_name)) %>%
  mutate(mb = map(data, ~ lm(perseverative_log ~ BPD, .)),
         mc = map(data, ~ lm(fconn ~ perseverative_log + BPD, .)),
         mmed = future_map2(mb, mc, ~ mediation::mediate(
           model.m=.x, model.y=.y, treat="BPD", mediator="perseverative_log", control.value=0, treat.value=1,
           robustSE=TRUE, use_speed=TRUE)), #sims=100, boot=TRUE,
         #tidy_mb=map(mb, tidy),
         tidy_med=map(mmed, tidy)
  )

#no age -> rl -> fconn relationship
persev_med <- dmat %>% unnest(tidy_med) %>% dplyr::select(-data, -mb, -mc, -mmed) %>%
  filter(grepl("acme_1", term))



dmat <- fconn_long %>% nest(data=c(-conn_name)) %>%
  mutate(mb = map(data, ~ lm(prob_switch_err ~ BPD, .)),
         mc = map(data, ~ lm(fconn ~ prob_switch_err + BPD, .)),
         mmed = future_map2(mb, mc, ~ mediation::mediate(
           model.m=.x, model.y=.y, treat="BPD", mediator="prob_switch_err", control.value=0, treat.value=1,
           robustSE=TRUE, use_speed=TRUE)), #sims=100, boot=TRUE,
         #tidy_mb=map(mb, tidy),
         tidy_med=map(mmed, tidy)
  )


#no  BPD prob_switch error relationship
probswitch <- dmat %>% unnest(tidy_med) %>% dplyr::select(-data, -mb, -mc, -mmed) %>%
  filter(grepl("acme_1", term) & p.value < .2)

cor_with_target(b_outputs_summary %>% as.data.frame(),
                target = c("perseverative", "prob_switch_err", "spont_switch_err",
                           "erratic_spont", "error_NOS", "explore_switch", "perseverative_log"),
                withvars = c("BPD", "AgeAtScan", "Female", "UPPS_negurg", "UPPS_lackprem",
                             "UPPS_lackpers", "UPPS_senseek", "UPPS_posurg", "BPQ_impuls",
                             "BPQ_instab", "BPQ_abandon", "BPQ_relations", "BPQ_self", "BPQ_suicide",
                             "BPQ_empti", "BPQ_anger", "BPQ_psycho", "BPQ_total"), pmin = .1, orderbyr = TRUE)

cor_with_target(b_outputs_summary %>% as.data.frame(),
                target = c("F", "LL", "AIC", "BIC", "R2", "alpha_win", "alpha_loss", "decay", "beta",
                           "alpha_win_transformed", "alpha_loss_transformed", "decay_transformed",
                           "beta_transformed", "alpha_win_ffx", "alpha_loss_ffx", "decay_ffx",
                           "beta_ffx", "alpha_win_transformed_ffx", "alpha_loss_transformed_ffx",
                           "decay_transformed_ffx", "beta_transformed_ffx"),
                withvars = c("BPD", "AgeAtScan", "Female", "UPPS_negurg", "UPPS_lackprem",
                             "UPPS_lackpers", "UPPS_senseek", "UPPS_posurg", "BPQ_impuls",
                             "BPQ_instab", "BPQ_abandon", "BPQ_relations", "BPQ_self", "BPQ_suicide",
                             "BPQ_empti", "BPQ_anger", "BPQ_psycho", "BPQ_total"), pmin = .1, orderbyr = TRUE)

#higher sensation seeking goes with stronger learning win rate
ggplot(b_outputs_summary, aes(x=alpha_win_transformed_ffx, y=UPPS_senseek)) + geom_point() +
  stat_smooth(method="lm")



