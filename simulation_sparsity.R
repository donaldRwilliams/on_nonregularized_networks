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


library(foreach)


# number of iterations
nsims =  1000
# conditions

df_cond <- read.csv("dat_cond_synth.csv")




iterations <-  nrow(df_cond) * nsims
#--title of progress bar
bar_title <- c("Patience")
pb <- tkProgressBar(max = iterations, title = bar_title)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress = progress)

# register for parallel computing
cl_1 <- makeCluster(6)
registerDoSNOW(cl_1)

# true matrix to sample from



stopCluster(cl_1)



results <-  foreach(j = seq_len(nrow(df_cond)),
                    .options.snow = opts) %:%
  foreach(i = 1:nsims, .errorhandling = "remove") %dopar% {

    df <- df_cond[j,]

    main <- BDgraph::bdgraph.sim(p = df$p, n = 500, prob = df$eta, b = df$df)
    X <- main$data

    ebic_time <- system.time(
      ebic <- qgraph::EBICglasso(qgraph::cor_auto(X), n = 500, gamma = 0.5)
    )

    bic_time <- system.time(
      bic <- GGMregress(X, IC = "BIC", method = "forward")
    )

    aic_time <- system.time(
      aic <- GGMregress(X, IC = "AIC", method = "forward")
    )

    boot_time <- system.time(
      boot <- boot_est(X, sims = 1000, alpha = 0.01)
    )

    row_names <- row.names(t(Diagnostic(ebic, main$G)))

    #############################
    ### binary classification ###
    #############################

    bc_ebic <- data.frame(score=  t(Diagnostic(ebic, main$G)), measure = row_names, model = "ebic")
    bc_bic_or <- data.frame(score=  t(Diagnostic(bic$pcor_or, main$G)), measure = row_names, model = "bic_or")
    bc_bic_and <- data.frame(score=  t(Diagnostic(bic$pcor_and, main$G)), measure = row_names, model = "bic_and")

    bc_aic_or  <- data.frame(score=  t(Diagnostic(aic$pcor_or, main$G)), measure = row_names, model = "aic_or")
    bc_aic_and <- data.frame(score=  t(Diagnostic(aic$pcor_and, main$G)), measure = row_names, model = "aic_and")

    bc_boot <- data.frame(score=  t(Diagnostic(boot$mat_selected, main$G)), measure = row_names, model = "boot")


    results <- rbind.data.frame(bc_ebic, bc_boot, bc_bic_and, bc_bic_or,
                                bc_aic_and, bc_aic_or)
    results$n <- 500
    results$p <- df$p
    results$size <- df$size
    results$sparsity <- df$eta
    results$df <- df$df
    results
  }

results_synthetic <- results

save(results_synthetic,file ="results_simulation_synth.Rdata")



results_simulation_synth <- do.call(rbind.data.frame, lapply(results_synthetic, function(x)  do.call(rbind.data.frame, x)))
write.csv(results_simulation_synth, file = "results_simulation_synth.csv")

