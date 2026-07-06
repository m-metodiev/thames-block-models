library(RcppArmadillo)
library(Rcpp)
library(Matrix)
library(ClusterR)
#sourceCpp("VBEM.cpp")

VBEM_rep <- function(X, K, n0=rep(1, K), eta0=matrix(1, K, K), zeta0=matrix(1, K, K), max_outer=100, eps_outer=1e-6, nb_rep=10, nb_rep_init=10, directed=FALSE,seed=2025) {
  
  maxcut <- log(.Machine$double.xmax) - log(K)
  mincut <- log(.Machine$double.xmin)

  n = dim(X)[1]
  tau_rep = list()
  ELBO_rep = list()
  max_ELBO = rep(0, nb_rep)
  
  for(rep in 1:nb_rep) {
    set.seed(seed + (rep-1))
    if(n>1000){
      #browser()
      reskmeans = MiniBatchKmeans(data=as.matrix(cbind(X, t(X))),
                                  clusters = K,
                                  batch_size = min(nrow(X),1000),
                                  num_init = nb_rep_init,
                                  max_iters = 1000,
                                  tol_optimal_init = 1e-6,
                                  verbose = TRUE,seed=seed + (rep-1))
      # res = kmeans(X, K, nstart = nb_rep_init)
      cl = predict_MBatchKMeans(as.matrix(cbind(X, t(X))),
                                reskmeans$centroids) - 1
    }else{
      res = kmeans(cbind(X,t(X)), K, nstart = nb_rep_init)
      cl = res$cluster
    }

    tau = matrix(0, n, K)
    tau[cbind(1:n, cl)]=1
    tau[tau<.Machine$double.xmin] <- .Machine$double.xmin
        
    maxcut <- log(.Machine$double.xmax) - log(K)
    mincut <- log(.Machine$double.xmin)
    res_SBM = VBEM(Matrix(X, sparse = TRUE), K, tau, n0, eta0, zeta0, mincut, maxcut, max_outer, eps_outer, as.numeric(directed))

    tau_rep[[rep]] = tau
    ELBO_rep[[rep]] = res_SBM
    max_ELBO[rep] = max(res_SBM)
  }
  best = which.max(max_ELBO) # find the run which led to the best ELBO
  #print(max_ELBO)
  #cat("best : ", best, "\n")
  tau = tau_rep[[best]]
  clSBM = apply(tau, 1, which.max)
  return(list(ELBO = ELBO_rep[[best]], cl=clSBM,tau=tau,max_ELBO=max_ELBO))
}
