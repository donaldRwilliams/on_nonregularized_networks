library(qgraph)

# +- 0.39
f <- function(eta, df, p ){
main <- BDgraph::bdgraph.sim(p = p, n = 50, prob =  eta, b = df)
pc <- cov2cor(main$K) * main$G
pc <- pc[upper.tri(pc)]
pc[pc != 0]
}

df <- seq(3, 50, by = 1)
eta <- c(0.1, 0.2, 0.3, .4)

res_i <- list()
res_j <- list()


###########################################################
######## solve for larger  partial correlations p = 20 ####
###########################################################
target_20_max <- unlist(replicate(1000, f(.5, df = 3, p = 20)))
target_20_max <- abs(quantile(target_20_max, probs = c(0.05)))



for(j in 1:length(eta)){
  print(j)
  for(i in 1:length(df)){
    e_01 <- unlist(replicate(1000, f(eta[j], df = df[i], p = 20)))
    e_01 <- abs(quantile(e_01, probs = c(0.05)))
    q_90 <- data.frame(q = e_01, eta = eta[j], df = df[i])
    res_i[[i]] <- q_90
  }
  res_j[[j]] <- res_i
}

unlist_i <- lapply(res_j, function(x) do.call(rbind.data.frame,x))
unlist(lapply(unlist_i, function(x) which.min(abs(x$q - target_20_max))))

df_sim <- df[unlist(lapply(unlist_i, function(x) which.min(abs(x$q - target_20_max))))]

dat_20_max <- data.frame(df = c(df_sim, 3), p = 20, eta = c(eta, 0.5), size = 0.39)



###########################################################
######## solve for smaller partial correlations p = 20 ####
###########################################################
target_20_min <- unlist(replicate(1000, f(.5, df = 25, p = 20)))
target_20_min <- abs(quantile(target_20_min, probs = c(0.05)))

for(j in 1:length(eta)){
  print(j)
  for(i in 1:length(df)){
    e_01 <- unlist(replicate(1000, f(eta[j], df = df[i], p = 20)))

    e_01 <- abs(quantile(e_01, probs = c(0.05)))
    q_90 <- data.frame(q = e_01, eta = eta[j], df = df[i])

    res_i[[i]] <- q_90
  }
  res_j[[j]] <- res_i
}



unlist_i <- lapply(res_j, function(x) do.call(rbind.data.frame,x))
unlist(lapply(unlist_i, function(x) which.min(abs(x$q - target_20_min))))

df_sim <- df[unlist(lapply(unlist_i, function(x) which.min(abs(x$q - target_20_min))))]

dat_20_min <- data.frame(df = c(df_sim, 25), p = 20, eta = c(eta, 0.5), size = 0.25)




###########################################################
######## solve for larger  partial correlations p = 10 ####
###########################################################
# match 0.39 (approximately)
target_10_max <- unlist(replicate(1000, f(.5, df = 11.5, p = 10)))
target_10_max <- abs(quantile(target_10_max, probs = c(0.05)))



for(j in 1:length(eta)){
  print(j)
  for(i in 1:length(df)){
    e_01 <- unlist(replicate(1000, f(eta[j], df = df[i], p = 10)))
    e_01 <- abs(quantile(e_01, probs = c(0.05)))
    q_90 <- data.frame(q = e_01, eta = eta[j], df = df[i])
    res_i[[i]] <- q_90
  }
  res_j[[j]] <- res_i
}

unlist_i <- lapply(res_j, function(x) do.call(rbind.data.frame,x))
unlist(lapply(unlist_i, function(x) which.min(abs(x$q - target_10_max))))

df_sim <- df[unlist(lapply(unlist_i, function(x) which.min(abs(x$q - target_10_max))))]

dat_10_max <- data.frame(df = c(df_sim, 11.5), p = 10, eta = c(eta, 0.5), size = 0.39)





###########################################################
######## solve for smaller  partial correlations p = 10 ####
###########################################################
# match 0.25 (approximately)
target_10_min <- unlist(replicate(1000, f(.5, df = 35, p = 10)))
target_10_min <- abs(quantile(target_10_min, probs = c(0.05)))



for(j in 1:length(eta)){
  print(j)
  for(i in 1:length(df)){
    e_01 <- unlist(replicate(1000, f(eta[j], df = df[i], p = 10)))
    e_01 <- abs(quantile(e_01, probs = c(0.05)))
    q_90 <- data.frame(q = e_01, eta = eta[j], df = df[i])
    res_i[[i]] <- q_90
  }
  res_j[[j]] <- res_i
}

unlist_i <- lapply(res_j, function(x) do.call(rbind.data.frame,x))
unlist(lapply(unlist_i, function(x) which.min(abs(x$q - target_10_min))))

df_sim <- df[unlist(lapply(unlist_i, function(x) which.min(abs(x$q - target_10_min))))]

dat_10_min <- data.frame(df = c(df_sim, 35), p = 10, eta = c(eta, 0.5), size = 0.25)


dat_cond_synt <- rbind.data.frame(dat_10_max, dat_10_min, dat_20_max, dat_20_min)

write.csv(dat_cond_synt, file = "dat_cond_synth.csv")
