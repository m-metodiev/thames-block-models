
#' Exact log marginal likelihood for the SBM
#'
#' @param g             a graph (the data)
#' @param a             first hyperparameter of the beta prior
#' @param b             second hyperparamter of the beta prior
#' @param n_dirichlet   hyperparameters of the dirichlet prior
#'
#' @returns a real value (the log marginal likelihood)
log_true_marglik = function(g, a, b, n_dirichlet){
  
  #browser()
  G = length(n_dirichlet)
  n <- ncol(g$X)
  
  # Generate all possible combinations using expand.grid
  set_values <- 1:G
  all_combinations <- expand.grid(rep(list(set_values), n))
  
  # Convert it to a matrix for easy access
  all_Cs <- as.matrix(all_combinations)
  
  # sum over all clusters using the log trick
  #browser()
  partial_log_marginal_liks  = sapply(1:nrow(all_Cs), function(i) compute_ICL(g, all_Cs[i,],a, b, n_dirichlet,i))
  true_marglik = log(sum(exp(partial_log_marginal_liks[!is.infinite(partial_log_marginal_liks)] - min(partial_log_marginal_liks[!is.infinite(partial_log_marginal_liks)])))) + 
    min(partial_log_marginal_liks[!is.infinite(partial_log_marginal_liks)])
  #browser()
  #log(rnorm())
  return(true_marglik)
}


#' Logarithm of the  joint density of the allocation vectors and the data
#'
#' @param g             a graph (the data)
#' @param Cmat          the allocation matrix (vector input is also accepted)
#' @param a             first hyperparameter of the beta prior
#' @param b             second hyperparameter of the beta prior
#' @param n_dirichlet   hyperparameters of the dirichlet prior
#' @param i             strictly for testing purposes
#'
#' @returns a real value (the log joint density)
compute_ICL = function(g, Cmat,a, b, n_dirichlet, i=NULL){
  G = length(n_dirichlet)
  n <- ncol(g$X)
  
  # a matrix instead of a vector is needed
  if(is.null(dim(Cmat))){
    Cmat = t(sapply(Cmat,function(x) diag(G)[, x]))
  }
  
  lps = 0
  for(g1 in 1:ncol(Cmat)){
    lps = lps + lgamma(1+sum(Cmat[,g1]))
  }
  lps = lps + lgamma(ncol(Cmat))

  lps = lps - lgamma(sum(sapply(1:g1,function(g1) 1+sum(Cmat[,g1]))))

  for(g1 in 1:ncol(Cmat)){
    for(k in 1:ncol(Cmat)){
      a = 1 + sum(sapply((1:n),function(i) sum(sapply((1:n)[-i],function(j) Cmat[i,g1]*Cmat[j,k]*g$X[i,j]))))
      b = 1 + sum(sapply((1:n),function(i) sum(sapply((1:n)[-i],function(j) Cmat[i,g1]*Cmat[j,k]*(1-g$X[i,j])))))
      lps = lps + lgamma(a) + lgamma(b) - lgamma(a+b)
    }
  }
  if(!is.null(i)){
    if(i %% 1000 == 0){
      print(i)
    }
  }
  
  return(lps)
}

#' Replicating the error of the greed package
#'
#' @param g             a graph (the data)
#' @param Cmat          the allocation matrix (vector input is also accepted)
#' @param a             first hyperparameter of the beta prior
#' @param b             second hyperparameter of the beta prior
#' @param n_dirichlet   hyperparameters of the dirichlet prior
#'
#' @returns a real value (the log joint density)
compute_ICL_greed = function(g, Cmat,a, b, n_dirichlet){
  G = length(n_dirichlet)
  n <- ncol(g$X)
  
  # a matrix instead of a vector is needed
  if(is.null(dim(Cmat))){
    Cmat = t(sapply(Cmat,function(x) diag(G)[, x]))
  }
  
  lps = 0
  for(g1 in 1:ncol(Cmat)){
    lps = lps + lgamma(1+sum(Cmat[,g1]))
  }
  lps = lps + lgamma(ncol(Cmat))

  lps = lps - lgamma(sum(sapply(1:g1,function(g1) 1+sum(Cmat[,g1]))))
  
  for(g1 in 1:ncol(Cmat)){
    for(k in 1:ncol(Cmat)){
      a = 1 + sum(sapply(1:n,function(i) sum(sapply((1:n),function(j) Cmat[i,g1]*Cmat[j,k]*g$X[i,j]))))
      b = 1 + sum(sapply(1:n,function(i) sum(sapply((1:n),function(j) Cmat[i,g1]*Cmat[j,k]*(1-g$X[i,j])))))
      lps = lps + lgamma(a) + lgamma(b) - lgamma(a+b)
    }
  }

  return(lps)
}