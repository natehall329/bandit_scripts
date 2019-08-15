
# bandit read of MFX and comparison to FFX ---------------------------------------------------
##this is an extension of tidy_VBA_output.R on the MFX structure.


basedir <- "~/Desktop/bandit_scripts/vba_output/"; setwd(basedir)

library(R.matlab)
library("rlist")
library(ggplot2)
library(cowplot)

identifier <- "_31-Jul-2019_.mat"
to_process <- list.files()[grepl("all_bandit_vba_output", list.files())]
identifier_f <- "_22-Jul-2019_.mat"
to_process_f <- list.files()[grepl(identifier_f, list.files())]

groupMat <- readMat(to_process)

subj_info <-  get(load("/mnt/ics/Michael/bpd_rest/cache/subjinfo_schaefer421_fsl_prewhitened_pearson.RData"))

# sub_ids <- sub("id_", "",sub(pattern = "_bandit_vba_.*", "",to_process))
sub_ids <- do.call(rbind,do.call(rbind,lapply(groupMat[[1]], function(x) x[[1]]["id",1,1]))) %>% data.frame(as.numeric(.)) %>% arrange(as.numeric...) %>% select(".")
sub_ids <- subj_info %>% filter(NUM_ID %in% sub_ids$.) %>% select(NUM_ID, SPECC_ID,BPD,Female)


# proc_df <- data.frame(fname = to_process, id = as.numeric(sub_ids)) %>% arrange(id)

uber_df <- data.frame()
fits <- data.frame(id = rep(NA,length(to_process_f)), R2_ffx=rep(NA,length(to_process_f)), class_acc_ffx = rep(NA,length(to_process_f)), R2_mfx = rep(NA,length(to_process_f)), class_acc_mfx = rep(NA,length(to_process_f)),
                   bic_ffx = rep(NA,length(to_process_f)), aic_ffx = rep(NA,length(to_process_f)), bic_mfx = rep(NA,length(to_process_f)), aic_mfx = rep(NA,length(to_process_f)),
                   beta_ffx = rep(NA,length(to_process_f)), theta1_ffx = rep(NA,length(to_process_f)), theta2_ffx = rep(NA,length(to_process_f)), theta3_ffx = rep(NA,length(to_process_f)), 
                   beta_mfx = rep(NA,length(to_process_f)), theta1_mfx = rep(NA,length(to_process_f)), theta2_mfx = rep(NA,length(to_process_f)), theta3_mfx = rep(NA,length(to_process_f))
                   ) 
value_list <- list()
plot_list <- list()
param_list <- list()

tictoc::tic()
pdf(file = "~/Desktop/bandit_scripts/ffx_mfx_checks.pdf", width = 11, height = 8 )
x<- get(load("~/Desktop/bandit_scripts/R/reinforce_sample.R"))
plot(x)

for(subn in 1:length(sub_ids$NUM_ID)){
  ###############
  #####MFX
  ###############
  sub_numid <- as.numeric(sub_ids[subn,1])
  sub_id <- sub_ids %>% filter(NUM_ID == sub_numid) %>% select(SPECC_ID) %>% as.character()
  cat(sub_id, "\n")
  
  
##get avg param estimates
    fits[subn,"beta_mfx"] <- as.numeric(groupMat[["posterior.sub"]][[subn]][[1]]["muPhi",1,1])
    fits[subn,c("theta1_mfx", "theta2_mfx", "theta3_mfx")] <- groupMat[["posterior.sub"]][[subn]][[1]]["muTheta",1,1]$muTheta[,1]
    
  
  output <- groupMat[["out.sub"]][[subn]][[1]][,,1]
  
  fits[subn, "id"] <- sub_id
  fits[subn,"R2_mfx"] <- output$fit["R2",1,1]
  fits[subn,"class_acc_mfx"] <- output$fit["acc",1,1]
  fits[subn,"bic_mfx"] <- output$fit["BIC",1,1]
  fits[subn,"aic_mfx"] <- output$fit["AIC",1,1]
  
  ###############
  #####FFX
  ###############
  sub <- paste0("id_", sub_numid, "_bandit_vba_output_valence_22-Jul-2019_.mat")
  subMat <- readMat(sub)
  
  ##get avg param estimates
  fits[subn, "beta_ffx"] <- subMat[["posterior"]]["muPhi",1,1]
  fits[subn,c("theta1_ffx", "theta2_ffx", "theta3_ffx")] <- subMat[["posterior"]]["muTheta",1,1]$muTheta[,1]
  
  ffx_output <- subMat[["out"]][,,1]
  
  fits[subn,"R2_ffx"] <- ffx_output$fit["R2",1,1]
  fits[subn,"acc_ffx"] <- ffx_output$fit["acc",1,1]
  fits[subn,"bic_ffx"] <- ffx_output$fit["BIC",1,1]
  fits[subn,"aic_ffx"] <- ffx_output$fit["AIC",1,1]
}


fits_melted <- fits %>% select(id, beta_ffx,theta1_ffx,theta2_ffx, theta3_ffx, beta_mfx,theta1_mfx, theta2_mfx, theta3_mfx) %>% melt(data = ., id.vars = c("id"), measure.vars = c("beta_ffx","theta1_ffx","theta2_ffx", "theta3_ffx", "beta_mfx","theta1_mfx", "theta2_mfx", "theta3_mfx"))

ggplot(fits_melted, aes(x = value)) + facet_wrap(~variable, nrow = 2) + geom_histogram()

# range(fits$theta1_mfx)
quantile(fits$beta_mfx)
quantile(fits$beta_ffx)

quantile(fits$theta1_mfx)
quantile(fits$theta1_ffx)

quantile(fits$theta2_mfx)
quantile(fits$theta2_ffx)

quantile(fits$theta3_mfx)
quantile(fits$theta3_ffx)


# range(fits$theta1_ffx)

beta_cor <- cor(fits$beta_ffx, fits$beta_mfx)
theta1_cor <- cor(fits$theta1_ffx, fits$theta1_mfx)
theta2_cor <- cor(fits$theta2_ffx, fits$theta2_mfx)
theta3_cor <- cor(fits$theta3_ffx, fits$theta3_mfx)

b <- ggplot(fits, aes(x = beta_ffx, y = beta_mfx)) + geom_point() + geom_smooth(method = lm) + ggtitle("correlation: ", beta_cor)
t1 <- ggplot(fits, aes(x = theta1_ffx, y = theta1_mfx)) + geom_point() + geom_smooth(method = lm) + ggtitle("correlation: ", theta1_cor)
t2 <- ggplot(fits, aes(x = theta2_ffx, y = theta2_mfx)) + geom_point() + geom_smooth(method = lm)+ ggtitle("correlation: ", theta2_cor)
t3 <- ggplot(fits, aes(x = theta3_ffx, y = theta3_mfx)) + geom_point() + geom_smooth(method = lm)+ ggtitle("correlation: ", theta3_cor)

cowplot::plot_grid(b,t1,t2,t3)

tictoc::toc()
dev.off()  
#   ffx_stats <- output[["suffStat"]]
#   # browser()
#   val_pe <- names(stats[c(25:35, 37, 40:47),1,1])
#   to_merge <- stats[c(25:35, 37, 40:47),1,]
#   
#   value_trace <- stats[,,1][["gx"]]
#   value_df <- data.frame(id = sub_id, val_A = value_trace[1,], val_B = value_trace[2,], val_C = value_trace[3,])
#   value_list[[sub]] <- value_df
#   
#   stats_df <- data_frame(trial = 1:300)
#   
#   stats_df$id <- sub_id
#   for(var in val_pe){
#     stats_df <- stats_df %>% mutate(!!var := to_merge[[var]][1,])
#   }
#   
#   #stats_df$PEunsigned.standardized <- as.numeric(scale(sqrt(stats_df$PEunsigned)))
#   
#   value_df$trial <- 1:300
#   z <- dplyr::select(value_df, -c(id))
#   zz <- melt(data = z, id.vars = "trial", measure.vars = c("val_A", "val_B", "val_C"))
#   
#   
#   value_plot <- ggplot(zz, aes(x = trial, y = value, color = variable)) + geom_line() +geom_point()+ geom_smooth() +scale_y_continuous(limits = c(0,1))+ ggtitle("Value Trace") + labs(y = "value")
#   
#   title <-  ggdraw() +
#     draw_label(
#       paste0("Subject ", sub_id, ". R-squared: ",fits[subn,2]),
#       fontface = 'bold',
#       x = 0,
#       hjust = 0
#     )
#   bottom_plot <- plot_grid(reinforcement_schedule, value_plot)#, ncol = 2)
#   
#   plot_list[[sub]] <- plot_grid(title, bottom_plot, ncol = 1, rel_heights = c(.1,1))
#   
#   
#   # ggplot(data = stats_df, aes(x = trial, y = PEunsigned)) + geom_smooth() + geom_point()
#   # ggplot(data = stats_df, aes(x = trial, y = PEunsigned.standardized)) + geom_smooth() + geom_point()
#   # 
#   # ggplot(data = stats_df, aes(x = trial, y = value)) + geom_smooth() + geom_point()
#   # ggplot(data = stats_df, aes(x = trial, y = delta)) + geom_smooth() + geom_point()
#   # ggplot(data = stats_df, aes(x = trial, y = PEsigned)) + geom_smooth() + geom_point()
#   # ggplot(data = stats_df, aes(x = trial, y = value.chosen)) + geom_smooth() + geom_point()
#   # # stats
#   
#   
#   # hist(stats_df$PEunsigned.standardized)
#   
#   uber_df <- rbind(uber_df, stats_df)
# }
# 
# r2 <- ggplot(fits, aes(x= R2)) + geom_histogram()
# class_acc <- ggplot(fits, aes(x= class_acc)) + geom_histogram()
# 
# bottom <- plot_grid(r2,class_acc)
# 
# title <-  ggdraw() +
#   draw_label(
#     "group-level R-squared and classification accuracy",
#     fontface = 'bold',
#     x = 0,
#     hjust = 0
#   )
# 
# 
# 
# plot_list[["total"]] <-plot_grid(title, bottom, ncol = 1, rel_heights = c(.1,1))
# plot_list
# 
# dev.off()
# 
# write_csv(uber_df, paste0("/mnt/ics/Michael/bpd_rest/behavior/bandit_VBA_tidy_",Sys.Date(), ".csv"))
# tictoc::toc()          