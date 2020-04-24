# devtools::install_github('AWKruijt/eMergeR')
library(eMergeR)
library(dplyr)
data_folder <- "~/Data_Analysis/bandit_scripts/Bandit_withrest"
dlist <- list.files(path=data_folder, pattern="\\d+", full.names = TRUE)
master_stimlist <- read.csv("~/Data_Analysis/bandit_scripts/Bandit_withrest/10/stims_10.csv")
  
#N.B. The Arew, Brew, and Crew denote whether the corresponding stimulus (fractal) would be rewarded if chosen.
#  On the other hand, reinforced1, reinforced2, and reinforced3 denote the whether the top, left, or right stimulus
#  would be rewarded: 4 = left, 5 = top, 6 = right
#  Thus, we need to transform things a bit to track the *identity* of the stimulus (which matters), not the position
#  of the stimulus (which doesn't). The showstim.ACC always denotes a reward for the chosen action, and showstim.RESP
#  denotes the position of this stimulus.
#  We need to map the choice back to A, B, C, then to map that to 1, 2, 3 for consistency with the y vector in VBA.
for (d in dlist) {
  df <- as.data.frame(edatR(Sys.glob(paste0(d, "/vrbl*.txt")))) %>% 
    mutate_at(vars(Arew, Brew, Crew, reinforced1, reinforced2, reinforced3, showstim.ACC, showstim.RESP), as.integer) %>%
    mutate_at(vars(topstim, leftstim, rightstim), list(~substr(., 1, 1))) %>%
    mutate(chosen_stimulus=case_when(
      showstim.RESP == 4 ~ leftstim,
      showstim.RESP == 5 ~ topstim,
      showstim.RESP == 6 ~ rightstim
    ),
    chosen_stimulus_number = dplyr::recode(chosen_stimulus, A=1, B=2, C=3)
    )

  # spot check on trnasformations  
  # df %>% select(Arew, Brew, Crew, reinforced1, reinforced2, reinforced3, 
  #               showstim.ACC, showstim.RESP, chosen_stimulus, chosen_stimulus_number,
  #               topstim, leftstim, rightstim) %>% head(n=20)

  #spot check stimulus rewards and positions against master (technically might want to check all three, but this is good enough)
  stopifnot(all.equal(df$Arew, master_stimlist$Arew)) #make sure master lookup matches what we have for this subject (consistent pseudo-random design)
  stopifnot(all.equal(df$reinforced1, master_stimlist$reinforced1)) #make sure master lookup matches what we have for this subject (consistent pseudo-random design)
  
  writeLines(df %>% pull(chosen_stimulus_number) %>% as.character(), con=file.path(d, "choices.txt"))
  writeLines(df %>% pull(showstim.ACC) %>% as.character(), con=file.path(d, "outcomes.txt"))
  
}
