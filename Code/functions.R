library(mvtnorm)
library(sna)
library(mclust)
library(Matrix)
library(RSpectra)
library(elbow)
library(RcppArmadillo)
library(Rcpp)
library(igraph)
library(ClusterR)
library(discoveR)

generateSBM<-function(n, PI, mu, directed=TRUE, selfloops=FALSE) {
  # n: number of nodes
  # PI: vector of cluster proportions
  # mu: matrix of connection probabilities
  # directed: 1 if directed, 0 otherwise
  # selfloops: 1 if selfloops are allowed, 0 otherwise
  
  
  Z = t(rmultinom(n, 1, PI))
  cl = apply(Z, 1, which.max)
  
  X = matrix(0, n, n)
  for(i in 1:n) {
    for(j in 1:n) {
      if(directed) {
        X[i, j] = rbinom(1, 1, mu[cl[i], cl[j]])
      } else {
        if(i <= j) {
          X[i, j] = rbinom(1, 1, mu[cl[i], cl[j]])
          X[j, i] = X[i, j]
        }
      }
    }
  }
  if(!selfloops) { #no self loops
    diag(X) = 0
  }
  return(list(X=as(X, "sparseMatrix"), Z=Z, cl=cl))
}

fastGenerateSBM<-function(n, PI, mu, directed=TRUE, selfloops=FALSE,shuffle=TRUE) {
  # n: number of nodes
  # PI: vector of cluster proportions
  # mu: matrix of connection probabilities
  # directed: 1 if directed, 0 otherwise
  # selfloops: 1 if selfloops are allowed, 0 otherwise
  
  Z = t(rmultinom(n, 1, PI))
  cl = apply(Z, 1, which.max)
  nc = table(cl)
  K = length(PI)
  print(paste0("cl: ",cl))
  
  if(length(unique(cl))==1){
    X = matrix(sample(0:1,size=n*n,replace=TRUE,prob=c(1-mu[cl[1],cl[1]],
                                                       mu[cl[1],cl[1]])),
               nrow=n,ncol=n)
  } else{
    X = matrix(0, n, n)
    r = 0
    for(k in 1:K) {
      c = 0
      for(l in 1:K) {
        # generate block (k, l)
        #print(t,k,c,l,mu)
        X[(r+1):(r+nc[k]), (c+1):(c+nc[l])] = matrix(rbinom(nc[k]*nc[l], 1, mu[k, l]), nc[k], nc[l])
        if(sum(is.na(X[(r+1):(r+nc[k]), (c+1):(c+nc[l])]))>0){
          browser()
        }
        c = c + nc[l]
      }
      r = r + nc[k]
    }
  }
    
  Z = Z[order(cl), ]
  cl = sort(cl)
  
  if(!directed) { # undirected
    X[lower.tri(X)] = t(X)[lower.tri(t(X))]
  }
  
  if(!selfloops) { # no self loops
    diag(X) = 0
  }
  
  # shuffle
  if (shuffle){
    id = sample(1:n)
    X = X[id, id]
    Z = Z[id, ]
    cl = cl[id]
  }
  
  return(list(X=as(X, "sparseMatrix"), Z=Z, cl=cl))
}

veryfastGenerateSBM<-function(n, PI, mu, directed=TRUE) {
  
  # n: number of nodes
  # PI: vector of cluster proportions
  # mu: matrix of connection probabilities
  # directed: 1 if directed, 0 otherwise
  
  # self loops not allowed in this code ! 
  
  Z = t(rmultinom(n, 1, PI))
  cl = apply(Z, 1, which.max)-1
  
  nc = table(cl)
  K = length(PI)
  
  edges = generatesbm(n, cl, mu, directed)
  
  g = graph_from_edgelist(edges+1, directed=directed)
  
  X = as_adjacency_matrix(g)
  diag(X) = 0
  
  return(list(X = X, Z=Z, cl=cl))
  return(edges)
  
}


gibbs_sampler <- function(Y, G, init=KMEANS, max_iterations=100, seed=2025,nb_rep_init=10)  {
  
  n = dim(Y)[1]
  
  tY = t(Y)
  
  maxcut <- log(.Machine$double.xmax) - log(G)
  mincut <- log(.Machine$double.xmin)
  
  # structures to store the samples of the Gibbs sampler
  #C = list()
  clusterList = list()
  tau = list()
  mu = list()
  lps = list()
  
  # init
  if(is.list(init)){
    print("manual init")
    clusterList[[1]] = init[[1]]
  } else{
    if(init == "KMEANS") {
      print("kmeans init")
      set.seed(seed)
      if(n>1000){
        reskmeans = MiniBatchKmeans(data=as.matrix(cbind(Y, t(Y))), 
                                    clusters = G, 
                                    batch_size = min(nrow(Y),1000), 
                                    num_init = nb_rep_init, 
                                    max_iters = 1000, 
                                    initializer = "kmeans++", 
                                    tol_optimal_init = 1e-6, 
                                    verbose=TRUE, seed=seed)
        clusterList[[1]] = predict_MBatchKMeans(as.matrix(cbind(Y, t(Y))),
                                                reskmeans$centroids) - 1
      } else{
        res = kmeans(cbind(Y, t(Y)), G, nstart = 10)$cluster
        clusterList[[1]] = res - 1
      }
      # C[[1]] = matrix(0, n, G)
      # C[[1]][cbind(1:n, cl)] = 1
      # C[[1]] = as(C[[1]], "sparseMatrix")
    }else if(init == "random"){
      print("random init")
      C = as(t(rmultinom(n, 1, rep(1/G, G))), "sparseMatrix")
      clusterList[[1]] = apply(C, 1, which.max) - 1
    }
  }
  tau[[1]] = rep(0, G) # cluster proportions
  mu[[1]] = matrix(0, G, G) # matrix of connection probabilities
  lps[[1]] = 0 # placeholder
  
  maxcut <- log(.Machine$double.xmax) - log(G)
  mincut <- log(.Machine$double.xmin)

  set.seed(seed)
  res = Gibbs_EMstep_full(Y, tY, n, G, clusterList[[1]], tau[[1]], mu[[1]], max_iterations, mincut, maxcut)
  
  # for(t in 2:(max_iterations+1)) {
  #   if((t-1)%%1000 == 0) {
  #     cat("iteration ", t-1, "\n")
  #   }
  #   
  #   res_t = Gibbs_EMstep(Y, tY, n, G, C[[t-1]], tau[[t-1]], mu[[t-1]], mincut, maxcut)
  #   tau[[t]] = res_t$tau
  #   mu[[t]] = res_t$mu
  #   C[[t]] = res_t$C
  #   lps[[t]] = res_t$lps
  # }
  
  return(list(clusterList=res[[1]],lps=simplify2array(res[[2]])))#, tau=tau[[1]], mu=mu[[1]], lps=lps[-1]))
}

