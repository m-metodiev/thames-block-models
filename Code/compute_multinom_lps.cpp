#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <rgen.h> 
// [[Rcpp::depends(rgen)]]


using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
List Gibbs_multinom_lps_full(int n, int G, umat clusterMat, mat probMat, int max_iterations) {
  List multinom_lpsList(max_iterations);
  //clusterList(0) = cluster; // save initialization
  urowvec cluster;
  for(int t=0; t<max_iterations; t++) {
    if((t-1)%1000 == 0) {
      std::cout << "iteration " << t-1 << std::endl;
    }
    cluster = clusterMat.col(t).t();
    double multinom_lps = 0;
    for(int i=0; i<n;i++){
      multinom_lps = multinom_lps + log(probMat(i,cluster(i)));
    }
    multinom_lpsList(t) = multinom_lps;
  }
  List resultList(2);
  resultList(0) = clusterMat;
  resultList(1) = multinom_lpsList;
  return(resultList);
}

