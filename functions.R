library(BDgraph)

# Kuismin, M., & Sillanp‰‰, M. J. (2016). Use of Wishart prior and simple extensions for 
# sparse precision matrix estimation. PloS one, 11(2), e0148171.
ebic = function(Y,rho,gamma){
  
  if(is.null(rho)) rho = seq(0.8,0.001,length.out=20)
  if(is.null(gamma)) gamma = .5
  
  n = nrow(Y); p = ncol(Y)
  
  S = cov(Y)
  
  EBIC = rep(0,length(rho))
  
  for(i in 1:length(rho)){
    
    Theta = glasso(S,rho[i])$wi
    
    E = matrix(0,p,p)
    
    E[Theta != 0] = 1
    
    E.card = sum(E[upper.tri(E)])
    
    log.like = (n/2)*(log(det(Theta)) - sum(diag(S%*%Theta)))
    
    EBIC[i] = -2*log.like + E.card*log(n) + 4*E.card*gamma*log(p)
    
  }
  
  return(EBIC)
  
}

cv_data <- function(X, k){
  n <- nrow(X)
  folds <- data.frame(id = 1:k,  sub = cvTools::cvFolds(n, K = k, type = "interleaved")$subsets)
  test_id <- lapply(1:k, function(x)  subset(folds,  id == x))
  train_id <- lapply(1:k, function(x)  subset(folds,  id != x))
  
  train_dat <-  lapply(train_id, function(x) X[x$sub,])
  test_dat <- lapply(train_id, function(x) X[-x$sub,])
  list(train_dat = train_dat, test_dat = test_dat)
}
cv_mle <- function(x, y){
  x <- scale(x, scale = F)
  y <- scale(y, scale = F)
  mle_cov <- t(x) %*% x / nrow(x) 
  inv_cov <- svd_inv_helper(y) 
  
  cv_error = -log(det(solve(inv_cov))) - sum(diag(mle_cov %*% inv_cov))
  
  
}


cv_glasso <- function(x, y, gamma, rho){
  x <- scale(x, scale = F)
  y <- scale(y, scale = F)
  mle_cov <- t(x) %*% x / nrow(x) 
  
  EBIC = ebic(y, rho, gamma)
  opt.index = which.min(EBIC)
  rhoEBIC = rho[opt.index]
  inv_cov = glasso(cov(y),rhoEBIC)$wi
  
  
  cv_error = -log(det(solve(inv_cov))) - sum(diag(mle_cov %*% inv_cov))
}

svd_inv_helper <- function(X){ 
  n = nrow(X)
  X <- scale(X, scale = F)
  mle_cov <- t(X) %*% X / n
  svd_decomp <- svd(mle_cov)
  inv_cov <- svd_decomp$v  %*% (1/ svd_decomp$d * t(svd_decomp$u))
  return(inv_cov)
}

boot_inv_helper <- function(X){
  n <- nrow(X)
  boot_sample <- X[sample(1:n, size = n, replace = T),  ]
  inv_cov <- svd_inv_helper(boot_sample)  
  pcor <- cov2cor(inv_cov) * -1
  diag(pcor) <- 1
  list(inv_cov = inv_cov, pcor = pcor, upper_tri = pcor[upper.tri(pcor)])
  
}

quantile_helper <- function(x, y){
  ifelse(x < 0 &  y > 0, 0, 1)
}

boot_est <- function(X, sims, alpha){
  mat_selected <- mat_mean <- matrix(0, ncol(X), ncol(X))
  lw_bound <- alpha / 2
  up_bound <- 1 -   lw_bound
  boot_results <-  t(replicate(sims,  boot_inv_helper(X)$upper_tri, simplify = T))
  
  
  quantiles <- t(apply(boot_results, MARGIN = 2,  quantile, probs = c(lw_bound, up_bound)))
  
  means <-  t(apply(boot_results, MARGIN = 2,  mean))
  
  mat_selected[upper.tri(mat_selected)] <- mapply(quantile_helper, x = quantiles[,1], y = quantiles[,2])
  mat_selected[lower.tri(mat_selected)] <- t(mat_selected)[lower.tri(mat_selected)]
  
  mat_mean[upper.tri(mat_mean)] <-  means
  mat_mean[lower.tri(mat_mean)] <- t(mat_mean)[lower.tri(mat_mean)]
  
  list(mat_selected = mat_selected, mat_mean =  mat_mean)
}


# Kuismin, M., & Sillanp‰‰, M. J. (2016). Use of Wishart prior and simple extensions for 
# sparse precision matrix estimation. PloS one, 11(2), e0148171.
  Diagnostic = function(ThetaEst,Theta){
  
  TN = ifelse(Theta[upper.tri(Theta)] == 0 & ThetaEst[upper.tri(ThetaEst)] == 0, 1, 0); TN = sum(TN) # True Negative
  
  FP = ifelse(Theta[upper.tri(Theta)] == 0 & ThetaEst[upper.tri(ThetaEst)] != 0, 1, 0); FP = sum(FP) # False Positive
  
  TP = ifelse(Theta[upper.tri(Theta)] != 0 & ThetaEst[upper.tri(ThetaEst)] != 0, 1, 0); TP = sum(TP) # True Positive
  
  FN = ifelse(Theta[upper.tri(Theta)] != 0 & ThetaEst[upper.tri(ThetaEst)] == 0, 1, 0); FN = sum(FN) # False Negatives
  
  Specificity = TN/(TN + FP) # aka True Negative rate
  
  Sensitivity = TP/(TP + FN) # aka True Positive rate
  
  Fallout = FP/(TN + FP) # aka False Positive rate
  
  Precision = TP/(TP + FP) # aka Positive predictive value
  
  MCC = (TP*TN - FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  
  lista = data.frame("Sp" = Specificity, "Sen" = Sensitivity, "Fall" = Fallout, "Pre" = Precision ,"MCC" = MCC)
  
  return(lista)
  
  }
  
  
or_helper <- function(x, y){
    ifelse(x * y == 0, max(abs(x), abs(y)), sqrt(abs(x) * abs(y)))
  }
  
  GGMregress <- function(X, IC, method){
    # scale data
    X <- scale(X, scale = T)
    n <- nrow(X)
    p <- ncol(X)
    
    mat1 <- mat2 <- mat_or <- matrix(0, p, p)  
    colnames(mat1) <- paste("X", 1:p, sep = "")
    colnames(X) <- 1:p
    
    test <- lapply(1:p, function(x) bestglm::bestglm(cbind.data.frame(X[,-x], X[,x]), 
                                                     method = method, IC = IC, intercept = F)$BestModel$coefficients)  
    
    for(i in 1:p){
      mat1[i,names(test[[i]])] <- test[[i]]
      
    }
    
    mat2[lower.tri(mat2)] <- sqrt(abs(mat1[lower.tri(mat1)]) * abs(t(mat1)[lower.tri(mat1)]))
    mat2[upper.tri(mat2)] <- t(mat2)[upper.tri(mat2)]
    
    mat_and <- sign(mat1) * mat2
    
    
    up <- mat1[upper.tri(mat1)]
    low <- t(mat1)[upper.tri(mat1)]
    
    sign_or <- sign(up)
    
    mat_or[upper.tri(mat_or)] <- mapply(or_helper,low, up) * sign_or
    mat_or[lower.tri(mat_or)] <- t(mat_or)[lower.tri(mat_or)]
    
    
    adj_and <- ifelse(mat_and == 0, 0, 1)
    adj_or  <- ifelse(mat_or == 0, 0, 1)
    
    list(pcor_and = mat_and, pcor_or = mat_or, adj_or = adj_or, adj_and = adj_and)
    
  }
  
  
mse <- function(est, true){
  sum((true[upper.tri(true)] - est[upper.tri(est)])^2)
  
  
}  
  
  
mae <- function(est, true){
 sum((true[upper.tri(true)] - est[upper.tri(est)]))
  
  
}  

cor_loss <- function(est, true){
  cor(true[upper.tri(true)], est[upper.tri(est)])
  
}



ordinal_gen <- function (n, input, n_levels) 
{
  if (is.list(input)) {
    if ("graph" %in% names(input)) {
      graph <- input$graph
    }
    else stop("'graph' not in input list.")
    if ("intercepts" %in% names(input)) {
      intercepts <- input$intercepts
    }
    else {
      intercepts <- rep(0, ncol(graph))
    }
  }
  else {
    if (!is.matrix(input)) {
      stop("'input' is not a matrix or list.")
    }
    graph <- input
    intercepts <- rep(0, ncol(graph))
  }
  if (!all(diag(graph) == 0 | diag(graph) == 1)) {
    graph <- cov2cor(graph)
  }
  diag(graph) <- 0
  Sigma <- cov2cor(solve(diag(ncol(graph)) - graph))
  Data <- mvtnorm::rmvnorm(n, sigma = Sigma)
  
  for (i in 1:ncol(Data)){
    Data[,i] <- as.numeric(cut(Data[,i],sort(c(-Inf,rnorm(n_levels-1),Inf))))
  }
  return(Data)
}

dat_gen <- function(x, n, p, cut = 0.05, type, likert_max = 5){
  # x: true
  # n: n
  # p: p
  s <- sample(1:ncol(x), p)
  colnames(x) <- 1:ncol(x)
  true <- as.matrix(Matrix::forceSymmetric(x[s,s]))
  true <- ifelse(abs(true) < cut, 0, true)
  diag(true) <- 1
  
  if(type == "cont"){
    dat <- as.data.frame(GeneNet::ggm.simulate.data(n, true))  
    colnames(dat) <- colnames(true)
  }else if(type == "ordinal"){
    dat <-  ordinal_gen(n, true, n_levels = likert_max)
    colnames(dat) <- colnames(true)
  }
  list(true = true, adj_mat= ifelse(true == 0, 0, 1), dat = dat)
}




