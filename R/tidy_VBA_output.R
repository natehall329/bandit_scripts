
# bandit read posterior ---------------------------------------------------
basedir <- "~/Desktop/bandit_scripts/vba_output/"; setwd(basedir)

library(R.matlab)
library("rlist")
library(ggplot2)
library(cowplot)

identifier <- "_22-Jul-2019_.mat"
to_process <- list.files()[grepl(identifier, list.files())]
sub_ids <- sub("id_", "",sub(pattern = "_bandit_vba_.*", "",to_process))
proc_df <- data.frame(fname = to_process_f, id = as.numeric(sub_ids)) %>% arrange(id)

uber_df <- data.frame()
fits <- data.frame(id = rep(NA,length(to_process)), R2=rep(NA,length(to_process)), class_acc = rep(NA,length(to_process))) 
value_list <- list()
plot_list <- list( )

contingency <- get(load("/mnt/ics/Michael/bpd_rest/behavior/bandit_contingency.RData"))
# colnames(contingency) <- c("")
reinforcement_schedule <- ggplot(contingency, aes(x = trial, y = value, color = variable)) + geom_line() +geom_point()+ geom_smooth() +scale_y_continuous(limits = c(0,1))+ ggtitle("Reinforcement Contingency (roughly)") + labs(y = "reward probability")
tictoc::tic()
pdf(file = "~/Desktop/bandit_scripts/value_vs_contingency.pdf", width = 11, height = 8 )

for(subn in 1:length(to_process)){
  
  
  sub <- as.character(proc_df[subn,1])
  
  sub_id <- as.character(str_split(sub(pattern = "_bandit_vba_.*", "",sub), "_")[[1]][2])
  cat(sub_id)
  
  
  subMat <- readMat(sub)
  # posterior <- subMat[["posterior"]][,,1]
  # str(posterior)
  # dim(posterior)
  # 
  output <- subMat[["out"]][,,1]
  fits[subn,] <- c(sub_id,subMat[["out"]][,,1]$fit[c(6,7)])
  # str(output)
  # 
  # output$fit
  stats <- output[["suffStat"]]
  # browser()
  val_pe <- names(stats[c(25:35, 37, 40:47),1,1])
  to_merge <- stats[c(25:35, 37, 40:47),1,]

  value_trace <- stats[,,1][["gx"]]
  value_df <- data.frame(id = sub_id, val_A = value_trace[1,], val_B = value_trace[2,], val_C = value_trace[3,])
  value_list[[sub]] <- value_df
  
  stats_df <- data_frame(trial = 1:300)
  
  stats_df$id <- sub_id
  for(var in val_pe){
    stats_df <- stats_df %>% mutate(!!var := to_merge[[var]][1,])
  }
  
  #stats_df$PEunsigned.standardized <- as.numeric(scale(sqrt(stats_df$PEunsigned)))
  
  value_df$trial <- 1:300
  z <- dplyr::select(value_df, -c(id))
  zz <- melt(data = z, id.vars = "trial", measure.vars = c("val_A", "val_B", "val_C"))

  
  value_plot <- ggplot(zz, aes(x = trial, y = value, color = variable)) + geom_line() +geom_point()+ geom_smooth() +scale_y_continuous(limits = c(0,1))+ ggtitle("Value Trace") + labs(y = "value")
  
  title <-  ggdraw() +
    draw_label(
      paste0("Subject ", sub_id, ". R-squared: ",fits[subn,2]),
      fontface = 'bold',
      x = 0,
      hjust = 0
    )
  bottom_plot <- plot_grid(reinforcement_schedule, value_plot)#, ncol = 2)
  
  plot_list[[sub]] <- plot_grid(title, bottom_plot, ncol = 1, rel_heights = c(.1,1))
  
  
  # ggplot(data = stats_df, aes(x = trial, y = PEunsigned)) + geom_smooth() + geom_point()
  # ggplot(data = stats_df, aes(x = trial, y = PEunsigned.standardized)) + geom_smooth() + geom_point()
  # 
  # ggplot(data = stats_df, aes(x = trial, y = value)) + geom_smooth() + geom_point()
  # ggplot(data = stats_df, aes(x = trial, y = delta)) + geom_smooth() + geom_point()
  # ggplot(data = stats_df, aes(x = trial, y = PEsigned)) + geom_smooth() + geom_point()
  # ggplot(data = stats_df, aes(x = trial, y = value.chosen)) + geom_smooth() + geom_point()
  # # stats
  
  
  # hist(stats_df$PEunsigned.standardized)
  
  uber_df <- rbind(uber_df, stats_df)
}

r2 <- ggplot(fits, aes(x= R2)) + geom_histogram()
class_acc <- ggplot(fits, aes(x= class_acc)) + geom_histogram()

bottom <- plot_grid(r2,class_acc)

title <-  ggdraw() +
  draw_label(
    "group-level R-squared and classification accuracy",
    fontface = 'bold',
    x = 0,
    hjust = 0
  )



plot_list[["total"]] <-plot_grid(title, bottom, ncol = 1, rel_heights = c(.1,1))
plot_list

dev.off()

write_csv(uber_df, paste0("/mnt/ics/Michael/bpd_rest/behavior/bandit_VBA_tidy_",Sys.Date(), ".csv"))
tictoc::toc()          