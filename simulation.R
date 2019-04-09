rm(list = ls())
##############################
##### Donald R. Williams #####
## Quantatitive Psychology  ##
### University, CA, Davis ####
##############################
#--projec title:
#--GGM: unregularized
# source("dat_gen.R")
# source("nonreg_models.R")
# source("glasso_func.R")
# source("loss_functions.R")
# source("ordinal_gen.R")
# source("or_rule.R")

source("functions.R")

library(parallel)
library(dplyr)
library(parcor)
library(qgraph)
library(SignifReg)
library(matrixStats)
library(tcltk)
library(doSNOW)

dat <- read.csv("dat_ptsd.csv")[,-1]



# number of iterations
nsims =  1000
# conditions 
p <- c(10, 20)
n <- c(100, 250, 500, 1000, 2500)
cond <- expand.grid(p, n)


df_cond <- rbind.data.frame(cond, cond)
colnames(df_cond) <- c("p", "n")

df_cond$type <- rep(c("cont", "ordinal"), each = 10)




iterations <-  nrow(df_cond) * nsims
#--title of progress bar
bar_title <- c("Patience")
pb <- tkProgressBar(max = iterations, title = bar_title)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress = progress)

# register for parallel computing 
cl_1 <- makeCluster(8)
registerDoSNOW(cl_1)

# true matrix to sample from


true <- corpcor::cor2pcor(corpcor::cor.shrink(dat))










results <-  foreach(j = seq_len(nrow(df_cond)), 
                      .options.snow = opts) %:%
    foreach(i = 1:nsims, .errorhandling = "remove") %dopar% {
      df <- df_cond[j,]
      
      sim_stuff <- dat_gen(x = true, n = df$n, p = df$p, type = df$type) 
      
      ebic_time <- system.time(
      ebic <- qgraph::EBICglasso(qgraph::cor_auto(sim_stuff$dat), n = df$n, gamma = 0.5)
      )
      
      bic_time <- system.time(
      bic <- GGMregress(sim_stuff$dat, IC = "BIC", method = "forward")
      )
      
      aic_time <- system.time(
      aic <- GGMregress(sim_stuff$dat, IC = "AIC", method = "forward")
      )
      
      boot_time <- system.time(
      boot <- boot_est(sim_stuff$dat, sims = 1000, alpha = 0.01)
      )
    
      ##############################
      ###### time data frame #######
      ##############################
      
      dat_time <- data.frame(score = c(ebic_time[3], bic_time[3], boot_time[3]), 
                             measure = "time", 
                             model = c("ebic", "regress", "boot"))
      
      
      #############################
      ### binary classification ###
      #############################
     
      bc_ebic <- data.frame(score=  t(Diagnostic(ebic, sim_stuff$adj_mat)), measure = row_names, model = "ebic")
      bc_bic_or <- data.frame(score=  t(Diagnostic(bic$pcor_or, sim_stuff$adj_mat)), measure = row_names, model = "bic_or")
      bc_bic_and <- data.frame(score=  t(Diagnostic(bic$pcor_and, sim_stuff$adj_mat)), measure = row_names, model = "bic_and")
      
      bc_aic_or  <- data.frame(score=  t(Diagnostic(aic$pcor_or, sim_stuff$adj_mat)), measure = row_names, model = "aic_or")
      bc_aic_and <- data.frame(score=  t(Diagnostic(aic$pcor_and, sim_stuff$adj_mat)), measure = row_names, model = "aic_and")
      
      bc_boot <- data.frame(score=  t(Diagnostic(boot$mat_selected, sim_stuff$adj_mat)), measure = row_names, model = "boot")
      
      
      
      #############################
      ##### mean squared error ####
      #############################
      dat_mse <- data.frame(score = c(mse(est = ebic, true = sim_stuff$true),
                                      mse(est = bic$pcor_or, true = sim_stuff$true),
                                      mse(est = bic$pcor_and, true = sim_stuff$true),
                                      mse(est = aic$pcor_or, true = sim_stuff$true),
                                      mse(est = aic$pcor_and, true = sim_stuff$true),
                                      mse(est = boot$mat_selected * boot$mat_mean, true = sim_stuff$true)),
                            measure = "mse",
                            model = c("ebic", "bic_or", "bic_and", "aic_or","aic_and", "boot"))
                            
      
      dat_cor <- data.frame(score = c(cor_loss(est = ebic, true = sim_stuff$true),
                                      cor_loss(est = bic$pcor_or, true = sim_stuff$true),
                                      cor_loss(est = bic$pcor_and, true = sim_stuff$true),
                                      cor_loss(est = aic$pcor_or, true = sim_stuff$true),
                                      cor_loss(est = aic$pcor_and, true = sim_stuff$true),
                                      cor_loss(est = boot$mat_selected * boot$mat_mean, true = sim_stuff$true)),
                            measure = "cor",
                            model = c("ebic", "bic_or", "bic_and", "aic_or","aic_and", "boot"))
      
      
      
     results <- rbind.data.frame(bc_ebic, bc_boot, bc_bic_and, bc_bic_or, 
                                 bc_aic_and, bc_aic_or, dat_time, 
                                 dat_mse, dat_cor)
     results$n <- df$n
     results$p <- df$p
     results$type <- df$type
     results
     }
results_emp <- results 

save(results_emp,file ="results_emp.Rdata")



results_simulation_emp <- do.call(rbind.data.frame, lapply(results_emp, function(x)  do.call(rbind.data.frame, x)))
write.csv(results_simulation_emp, file = "results_simulation_emp.csv")

