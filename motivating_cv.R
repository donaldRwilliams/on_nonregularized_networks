rm(list = ls()) 
source("functions.R")
 
main <- bdgraph.sim(p = 20, n = 200)

write.csv(main$sigma, file = "motivating_cov.csv")

# number of simulations
nsims <- 500

# sample sizes
n <- c(50, 100, 250, 500, 1000, 2500)

# penalities
rho <- seq(0.001, .5, length.out = 100)


list_i <- list()
list_j <- list()


sigma <- read.csv("motivating_cov.csv")[,-1]


for(j in 1:length(n)){
for(i in 1:nsims){
  print(c("condition", j, "iteration", i))
  # generate data
  dat <- MASS::mvrnorm(n[j], rep(0, 20), Sigma = cov2cor(main$sigma))
  # create subsets
  test <- cv_data(dat, k = 5)
  # fit glasso
  gl_0 <- mean(mapply(cv_glasso, test$test_dat, test$train_dat, MoreArgs = list(gamma = 0, rho = rho)))
  gl_05 <- mean(mapply(cv_glasso, test$test_dat, test$train_dat, MoreArgs = list(gamma = 0.5, rho = rho)))
  # maximum likelihood estimation
  mle <- mean(mapply(cv_mle, test$test_dat, test$train_dat))
  list_i[[i]] <- data.frame(gl_0 =  gl_0, gl_05 = gl_05, mle = mle, n = n[j])
}
 list_j[[j]] <- list_i
}


results <- do.call(rbind.data.frame,  
                   lapply(1:length(n), function(x)  
                     do.call(rbind.data.frame, list_j[[x]])))

write.csv(results, file = "results_motivating_cv.csv")


 
for(j in 1:length(n)){
  for(i in 1:nsims){
    print(c("condition", j, "iteration", i))
    # generate data
    dat <- MASS::mvrnorm(n[j], rep(0, 20), Sigma = cov2cor(as.matrix(sigma)))
    # fit glasso
    EBIC <- ebic(dat, rho = rho, gamma = 0)
    opt.index = which.min(EBIC)
    rhoEBIC = rho[opt.index]
    inv_cov = glasso(cov(dat),rhoEBIC)$wi
    gl_0 <- max(eigen(inv_cov)$values) / min(eigen(inv_cov)$values)
    
    
    EBIC <- ebic(dat, rho = rho, gamma = .5)
    opt.index = which.min(EBIC)
    rhoEBIC = rho[opt.index]
    inv_cov = glasso(cov(dat),rhoEBIC)$wi
    gl_05 <- max(eigen(inv_cov)$values) / min(eigen(inv_cov)$values)
    
    # fit mle 
    mle <- svd_inv_helper(dat)
    mle <- max(eigen(mle)$values) / min(eigen(mle)$values)
    
    list_i[[i]] <- data.frame(gl_0 =  gl_0, gl_05 = gl_05, mle = mle, n = n[j])
  }
  list_j[[j]] <- list_i
}

true_range <- max(eigen(solve(cov2cor(main$sigma)))$values) / min(eigen(solve(cov2cor(main$sigma)))$values)


results <- do.call(rbind.data.frame,  
                   lapply(1:length(n), function(x)  
                     do.call(rbind.data.frame, list_j[[x]])))


write.csv(results, file = "results_motivating_eigen.csv")



