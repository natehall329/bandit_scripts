
# inspect VBA MFX output and look for associations with connectivity metrics. --------------------------------------------------

pacman::p_load(R.matlab, tidyverse, dependlab, sigmoid, beepr, psych,MuMIn)

# bandit_dir<- "/Users/nth7/Desktop/bandit_scripts"
# mount_dir <- "/mnt/ics"

bandit_dir<- "~/Data_Analysis/bandit_scripts"
mount_dir <- "/Users/Shared/ics"


out <-  readMat(paste0(bandit_dir, "/vba_output/all_bandit_vba_output_valence_31-Jul-2019_.mat")) #takes some time.
beepr::beep()

sub <- out[["out.sub"]]
R2_cutoff <- .25 # variable defining which subjects should be dropped due to poor fit. This was chosen somewhat arbitrarily by sorting subjects on Rquared and looking at their model fits via VBA_Redisplay and looking for the model just completely missing the mark. 
# str(sub)



# grab output and info on posteriors from giant output --------------------


bandit_outputs <- data.frame() # assorted model output from VBA MFX. not sure what all of this means at this point
posteriors <- data.frame() # posterior info from VBA MFX

for(i in 1:length(sub)){
  df <- sub[[i]][[1]][,,1]
  id <- as.numeric(df[["id"]]); print(id)
  fits <- df[["fit"]][,,1] %>% do.call(rbind, .) %>% data_frame() %>% mutate(fit_index = c(names(df[["fit"]][,,1]))) 
  colnames(fits)[1] <- "score"
  fits <- fits %>% spread(fit_index, score) %>% mutate(id = id)
  x <- df[["suffStat"]][,,1][-c(21,22,23)] #drop SigmaX, ODE.posterior, and ODE.suffStat as these are all lists and I'm not even sure what they represent. 
  # do.call(rbind, x)
  # x[["value.not.chosen"]]
  # str(x)
  # pull all elements that have one row. These will be easy to bind.
  sstats <- x[c(#"F", #drop F for now since I'm not quite sure what it does, doesn't seem crucially important. for single values we can just replicate them to fit our needs.
    "dy2", "dx2", "dphi", "Ssigma", "Sphi", "Stheta", "Salpha", "SX", "SX0", "logL", "div", "delta", 
    "PEchosen.pos", "PEchosen.neg", "PEunsigned", "PEchosen", 
    "value", "value.diff", "value.chosen", "value.chosen.diff", "vtplus1.chosen.diff","vtplus1.max", "value.chosen.diff.standardized", "PEchosen.standardized", 
    "reward.stake", "reward.stake.mc", "stake", "stake.mc", 
    "win.stay.10.prob", "win.stay.25.prob", "win.stay.50.prob", 
    "loss.stay.10.prob", "loss.stay.25.prob", "loss.stay.50.prob")]
  
  ss_trans <- lapply(sstats, function(x){
    x <- t(x)
    if(nrow(x) != 300){
      x <- rep(x,300)
      x
    } else {
      x
    }
    # ifelse(nrow(x) == 300, x, rep(x,300))
    # x
  }) 
  
  df_ss <- do.call(cbind, ss_trans) %>% data.frame() %>% mutate(id = id)
  colnames(df_ss) <- c(names(sstats), "id")
  # str(df_ss)
  
  # pull all elements with multiple rows. may take some manual intervention
  
  sstats_m <- x[c("gx", "vy", "muX", #"dx0", "dtheta", #drop these, they dont add much and make things confusing. 
                  "value.not.chosen", "vtplus1", "vtplus1.not.chosen")]
  # str(sstats_m)
  ss_trans_m <- lapply(sstats_m, function(x){
    x <- t(x)
    if(nrow(x) == 300){
      x
    } else if(nrow(x) == 1) {
      # x <- t(x)
      # x <- rep(x,300)
      x
    } else{
      x <- rbind(x, rep(0,ncol(x)))
      x
    }
    # ifelse(nrow(x) == 300, x, rep(x,300))
    # x
  })
  
  df_ss_m <- do.call(cbind, ss_trans_m) %>% data.frame() %>% mutate(id = id)
  colnames(df_ss_m) <- c(paste0("gx_", seq(1,3,1)),
                         paste0("vy_", seq(1,3,1)),
                         paste0("muX_", seq(1,4,1)),
                         paste0("value.not.chosen_", seq(1,2,1)),
                         paste0("vtplus1_", seq(1,3,1)),
                         paste0("vtplus1.not.chosen_", seq(1,2,1)),
                         "id")
  
  df_out <- df_ss %>% left_join(fits, by = "id") 
  df_out <- cbind(df_out, df_ss_m)
  df_out <- df_out[,-length(df_out)]
  bandit_outputs <- rbind(bandit_outputs,df_out)
  
  # grab info from posteriors -----------------------------------------------
  
  names(out)
  
  posterior.sub <- data.frame(out[["posterior.sub"]][[i]][[1]][,,1]["muPhi"], t(out[["posterior.sub"]][[i]][[1]][,,1][["muTheta"]]), id)
  colnames(posterior.sub) <- c("softmax_temp", "alpha_win", "alpha_loss", "lambda", "id")
  posteriors <- rbind(posteriors,posterior.sub)
  
}





# Rsquared distributions. Consider dropping really bad fits. --------------


hist(unique(bandit_outputs$R2)) #generally looks acceptable to me...
# quantile(bandit_outputs$R2)
quantile(bandit_outputs$R2,seq(.1,1,.1))


# look at RL parameters correlate with one another and predict group status and age and combine with connectivity metrics --------


## these are most of the requisite dfs from the resting state analysis. 
basedir <- paste0(mount_dir,"/Michael/bpd_rest")
load(paste0(basedir, "/output.files/results_005_200.RData"))  #results_005. results of the ridge regression that fell below a p-value of .01. Though not all subnuclei of the amygdala were signifcant at this step, we included all in the GIMME analysis.
scaled <- get(load(paste0(basedir, "/cache/scaled_inputs_all_200.RData"))) #scaled. large list with fl and soc dfs to be input to logistic ridge
load(file.path(basedir, "cache/subjinfo_schaefer421_fsl_prewhitened_pearson.RData")) #subj_info
# load(file.path(basedir, "behavior/sr_behav_outcomes.RData")) #contains BPQ, DERS, emo-gender-conflict, 

colnames(subj_info)[1] <- "id"

rl_subs <- subj_info %>% left_join(posteriors, by = "id") %>% data.frame() %>% select(contains("ID"), BPD, AgeAtScan, alpha_win,alpha_loss, softmax_temp, lambda) # combine relevant subject info with params from RL model 
rl_subs %>% filter(is.na(alpha_win)) # who is missing bandit fits?


# preliminary checking of parameter distributions ------------------------

# I've commented this out to keep parameter fits relatively gaussian for analyses below.

# hist(rl_subs$alpha_win)
# rl_subs$alpha_win <- sigmoid(rl_subs$alpha_win)
# hist(rl_subs$alpha_win)
# 
# hist(rl_subs$alpha_loss)
# rl_subs$alpha_loss <- sigmoid(rl_subs$alpha_loss)
# hist(rl_subs$alpha_loss)
# 
# hist(rl_subs$lambda)
# rl_subs$lambda <- sigmoid(rl_subs$lambda)
# hist(rl_subs$lambda)
# 
# hist(rl_subs$softmax_temp)
# rl_subs$softmax_temp <- exp(rl_subs$softmax_temp)
# hist(rl_subs$softmax_temp)

cor_heatmap(rl_subs[,-c(1:3)])

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



# drop subjects with poor fit ---------------------------------------------

r2 <- bandit_outputs %>% select(id, R2) %>%group_by(id) %>% summarise(R2 = mean(R2)) #
# r2 %>% left_join(rl_subs, by = "id") %>% arrange(-R2)
rl_subs <- rl_subs %>% left_join(r2, by = "id") 
rl_subs$R2_drop <- ifelse((rl_subs$R2 < R2_cutoff) |is.na(rl_subs$R2) , 1,0) 
# rl_subs %>% arrange(-R2)
# table(rl_subs$BPD,rl_subs$R2_drop)

rl_subs <- cbind(rl_subs,neural_mediators[,16:22]) #the subsetting pulls just frontolimbic FC edges.
rl_subs$BPD_fac <- factor(rl_subs$BPD, levels = c(0,1), labels = c("HC", "BPD"))


# look at correlation matrix: FC x model params ---------------------------

cor_heatmap(rl_subs[which(rl_subs$R2_drop != 1),-c(1:3,11)])

## check out age-related interactions. look for similarities between agexgroup in model parameters and agexgroup connectivity.
rl_age <- rl_subs %>% filter(R2_drop == 0) %>% select(AgeAtScan, BPD, alpha_loss, alpha_win, lambda, softmax_temp, id) 
# toplot <- reshape2::melt(rl_age, id.vars = c("AgeAtScan", "BPD"))
# toplot$BPD <- factor(toplot$BPD, levels = c(0,1), labels = c("HC", "BPD"))
# 
# ggplot(toplot, aes(x = AgeAtScan, y= value, color = BPD)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~variable, scales = "free_y")

# looks like a few minor outliers could be messing with things most specifically wrt alpha_win and temperature. Try pulling in some of these values and replotting.
rl_age$alpha_loss_winsor <- winsor(rl_age$alpha_loss, trim = .1)
rl_age$alpha_win_winsor <- winsor(rl_age$alpha_win, trim = .1)
rl_age$lambda_winsor <- winsor(rl_age$lambda, trim = .1)
rl_age$softmax_temp_winsor <- winsor(rl_age$softmax_temp, trim = .1)

toplot <- reshape2::melt(select(rl_age, -id), id.vars = c("AgeAtScan", "BPD"))
toplot$BPD <- factor(toplot$BPD, levels = c(0,1), labels = c("HC", "BPD"))

ggplot(toplot, aes(x = AgeAtScan, y= value, color = BPD)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~variable, scales = "free_y", ncol = 4)

# looks like potentially interesting age-related increases in alpha_win in the BPD group, which is the opposite in the control group. This is the flipped effect seen between the mPFC and VS.
# temperature decreases in HC group but not in BPD folks
# alpha_loss and lambda (decay) increase in HCs, flat in BPD.

# merge winsorized values back into master df
rl_subs <- rl_subs %>% left_join(select(rl_age, contains("_winsor"), "id"), by = "id")


ggplot(filter(rl_subs, R2_drop ==0), aes_string(x = "AgeAtScan", y= "`84_220`", color = "BPD_fac")) + geom_point() + geom_smooth(method = "lm") 
ggplot(filter(rl_subs, R2_drop ==0), aes_string(x = "alpha_win_winsor", y= "`84_220`")) + geom_point() + geom_smooth(method = "lm") # okay, maybe not a whole lot here.

m1 <- lm(alpha_win_winsor ~ `84_220`, data = filter(rl_subs, R2_drop == 0))
summary(m1)

m1 <- lm(alpha_win_winsor ~ `84_220`* R2, data = filter(rl_subs, R2_drop == 0))
summary(m1)


m2 <- lm(alpha_win_winsor ~ `90_215`* R2, data = filter(rl_subs, R2_drop == 0))
summary(m2)

# not much here, let's move on for now.


# grab CS-GIMME outputs ---------------------------------------------------
load(paste0(basedir, "/cache/directed_df.RData"))

drdf <- directed_df[,c(5:39, 106)] #hacky way of pulling fronto-limbic EC results. This includes group and subgroup-level edges and in and out degree estimated from GIMME.

rl_subs <- cbind(rl_subs, drdf)  
# names(rl_subs)
# all(rl_subs[,59] == rl_subs[,4]) # check to make sure the hacky cbind looks good
rl_subs <- rl_subs[,-59]

drop_cols <- c("id", "SPECC_ID", "Luna_ID", "R2_drop", "BPD_fac")
rl_parms <- c("alpha_win", "alpha_loss", "lambda", "softmax_temp")

# look at corrs between model parameters and anything related to the VS. 
cor_heatmap(rl_subs %>% filter(R2_drop == 0 ) %>% select(contains("215"), contains("220"), rl_parms, "BPD")) # not a whole lot other than an interesting flare-up amongst left -> right VS connectivity and temperature.



# look at b files, which contain behavioral errors of interest ---------------------------------------------------------
bs <- data.frame()

for(i in r2$id){
  # i <- 2
  # b <- read.csv(paste0("~/Desktop/bandit_scripts/b_outputs_subject",i,".csv"))
  b <- read.csv(paste0(bandit_dir,"/b_outputs_subject",i,".csv")) %>% mutate(id = i)
  
  b_sum <- b %>% select(id, contains("err"), perseverative, explore_switch) %>% summarise(perseverative = sum(perseverative),
                                                                                          prob_switch_err = sum(prob_switch_err),
                                                                                          spont_switch_err = sum(spont_switch_err),
                                                                                          erratic_spont = sum(erratic_spont),
                                                                                          error_NOS = sum(error_NOS),
                                                                                          explore_switch = sum(explore_switch)
  ) %>% mutate(id = i)
  
  bs <- rbind(bs, b_sum)
  # i <- 97
  
}


cor_heatmap(bs)
# bs <- bs <- left_join(select)

# fa(select(bs,-"id"), nfactors = 2, rotate = "Promax")

rl_subs <- rl_subs %>% left_join(bs, by = "id") #%>% 

errors <- colnames(bs)[-7]

cor_heatmap(rl_subs %>% filter(R2_drop == 0) %>% select(contains("215"), contains("220"), rl_parms, errors, BPD))

## probabalistic switch error: when subject previously chose the highest value option, and this was not rewarded, which led the subeject to switch to another stimulus. This seems to correlate input of dACC (90) to VS (215)


# group-related plots for group and group x age interactions for errors of interest. --------


bs <- bs %>% left_join(select(subj_info, id,BPD, AgeAtScan))

bs_melt <- reshape2::melt(bs, id.vars = c("id", "BPD", "AgeAtScan"))
bs_melt$BPD <- factor(bs_melt$BPD, levels = c(0,1), labels = c("HC", "BPD"))

ggplot(bs_melt, aes(x = AgeAtScan, y = value, color = BPD)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~variable, scales = "free_y") 



# do one final loop over errors and model params and run simple lms --------

loop_over <- c(paste0(rl_parms, "_winsor"), errors)
conn_metrics <- colnames(rl_subs)[c(12:19, 24:58)]
results_rl <- list()

bandit_outputs <- bandit_outputs %>% mutate(PEchosen_tmin1 = ifelse(is.na(lag(PEchosen)), 0, lag(PEchosen)))#,
# PEunsigned_tmin1 = ifelse(is.na(lag(PEunsigned)), 0, lag(PEunsigned)),)
rl_subs <- rl_subs %>% left_join(select(bandit_outputs, id, PEchosen_tmin1), by = "id")


rl_subs_single_obs <- rl_subs %>% group_by(id, AgeAtScan, BPD, `86_220`, `84_220`, `90_215`, `89_220`, alpha_loss_winsor, alpha_win_winsor,lambda_winsor, softmax_temp_winsor,  ) #%>% summarise(e86_220 = mean(`86_220`), )

rl_subs_single_obs <- rl_subs %>% select(-PEchosen_tmin1) %>% distinct()

pdf(paste0(bandit_dir, "/heatmaps_models.pdf"), width = 11, height = 8)
for(i in loop_over){
  
  forms_l <- list()
  plots_form <- list()
  coefs_l <- list()
  aic_df <- data.frame()
  big_coef_df <- data.frame()
  
  for(j in conn_metrics[grepl("215", conn_metrics) | grepl("220", conn_metrics)]){ #for just VS-related effects
    
    # 
    
    # for(j in conn_metrics){ # for all effects
    # if(i %in% errors){
    #   
    #   #unsure how to model this properly. Here are a few ideas.
    #   forms_l[["form1"]] <- formula(paste0(i, "~ `", j,"`+ BPD + AgeAtScan + PEchosen_tmin1"))
    #   forms_l[["form2"]] <- formula(paste0(i, "~ `", j,"` * PEchosen_tmin1+ BPD + AgeAtScan "))
    #   forms_l[["form3"]] <- formula(paste0(i, "~ `", j,"` * BPD + AgeAtScan + PEchosen_tmin1"))
    #   # forms_l[["form4"]] <- formula(paste0(i, "~ `", j,"` + BPD * AgeAtScan + PEchosen_tmin1"))
    #   # forms_l[["form5"]] <- formula(paste0(i, "~ `", j,"` * BPD * PEchosen_tmin1 + AgeAtScan"))
    #   # forms_l[["form6"]] <- formula(paste0(i, "~ `", j,"` * PEchosen_tmin1 * AgeAtScan + BPD"))
    #   # forms_l[["form7"]] <- formula(paste0(i, "~ `", j,"` * PEchosen_tmin1 * AgeAtScan * BPD"))
    # } else{
    #   #unsure how to model this properly. Here are a few ideas.
    #   forms_l[["form1"]] <- formula(paste0(i, "~ `", j,"` + BPD + AgeAtScan "))
    #   forms_l[["form2"]] <- formula(paste0(i, "~ `", j,"` * BPD + AgeAtScan "))
    #   forms_l[["form3"]] <- formula(paste0(i, "~ `", j,"` *  AgeAtScan + BPD"))
    #   
    #   forms_l[["form1"]] <- formula(paste0(i, "~ `", j,"` "))#+ BPD + AgeAtScan "))
    #   # forms_l[["form2"]] <- formula(paste0(i, "~ `", j,"` "))#* BPD + AgeAtScan "))
    #   # forms_l[["form3"]] <- formula(paste0(i, "~ `", j,"` "))#*  AgeAtScan + BPD"))
    #   
    # }
    
    # unsure if the above is helpful. just try very simple
    
    forms_l[["form1"]] <- formula(paste0(i, "~ `", j,"`"))# + AgeAtScan "))
    forms_l[["form2"]] <- formula(paste0(i, "~ `", j,"` + BPD"))# + AgeAtScan "))
    forms_l[["form3"]] <- formula(paste0(i, "~ `", j,"` * BPD"))# + AgeAtScan "))
    forms_l[["form4"]] <- formula(paste0(i, "~ `", j,"` + BPD + AgeAtScan "))
    forms_l[["form5"]] <- formula(paste0(i, "~ `", j,"` + BPD * AgeAtScan "))
    forms_l[["form6"]] <- formula(paste0(i, "~ `", j,"` * BPD * AgeAtScan "))
    
    coef_df <- data.frame()
    
    for(f in 1:length(forms_l)) { #fit model and pull coefficient table
      fo <- forms_l[[f]]
      mod <- lm(fo, data = rl_subs_single_obs %>% filter(R2_drop == 0))
      out <- summary(mod)
      coefs <- out$coefficients%>% data.frame()%>% rownames_to_column() %>% mutate(form = paste0("form_", f), bandit_index = i, AICc = AICc(mod), conn_metric = j) %>% filter(rowname != "(Intercept)")
      colnames(coefs)[1:5] <- c("parameter", "est", "SE", "t", "p")
      coefs$parameter <- gsub("`", "", sub(j, "conn_index", coefs$parameter))
      coef_df <- rbind(coef_df,coefs)
      
      # interactions::probe_interaction(mod, pred = ``)
    } # end loop over formulas
    this.aic <- coef_df %>% group_by(form) %>% summarise(mean(AICc)) %>% mutate(conn_metric = j, bandit_index = i)
    aic_df <- rbind(aic_df, this.aic)
    big_coef_df <- rbind(big_coef_df, coef_df)
    
  } #end inner loop over conn_metrics
  
  colnames(aic_df)[2] <- "AICc"
  ## look at average AICc across connectivity metrics
  aic_df <- aic_df %>% group_by(form) %>% summarise(mean_AICc = mean(AICc))
  winning_model <- as.character(aic_df[which(aic_df$mean_AICc == min(aic_df$mean_AICc)),1])
  # ggplot(aic_df, aes(x = AICc)) + geom_histogram() + facet_wrap(~form)

  for_plotting <- big_coef_df
  # for_plotting$t <- ifelse(for_plotting$p > .05, NA, for_plotting$t)
  for_plotting <- for_plotting %>% filter(!grepl("subgroup",conn_metric))
  
  ### make frames for p < .05, p < .01, p < .005
  for_plotting$na.05 <- ifelse(for_plotting$p < .05 & for_plotting$p > .01, TRUE, FALSE)
  for_plotting$na.01 <- ifelse(for_plotting$p <= .01 & for_plotting$p >.005, TRUE, FALSE)
  for_plotting$na.005 <- ifelse(for_plotting$p <= .005, TRUE, FALSE)
  
  frames.05 <- for_plotting %>% filter(na.05)
  frames.01 <- for_plotting %>% filter(na.01)
  frames.005 <- for_plotting %>% filter(na.005)
  
  x <- ggplot(data = for_plotting) +
    # data = filter(big_coef_df, form == "form_1"),
    geom_tile(aes(x = parameter, y = conn_metric, fill = t)) + scale_fill_viridis() + facet_wrap(~form) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle(paste0(i, " - winning model based on AICc comparison: ", winning_model)) 
  
  tmp <- ggplot_build(x)$data[[1]][2,]  # get the first data point from geom_raster
  width <- tmp$xmax - tmp$xmin  # calculate the width of the rectangle
  height <- tmp$ymax - tmp$ymin  # calculate the height of the rectangle
  
  x <- x + geom_tile(data = frames.05, aes(x = parameter, y = conn_metric), width = width, height = height, color = "black", fill = NA, size=1) +
    geom_tile(data = frames.01, aes(x = parameter, y = conn_metric), width = width, height = height, color = "orange", fill = NA, size=1) +
    geom_tile(data = frames.005, aes(x = parameter, y = conn_metric), width = width, height = height, color = "red", fill = NA, size=1)
  
  print(x)
}
dev.off()
beepr::beep()

