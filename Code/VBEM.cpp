#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;
using namespace arma;


double digamma(double x)
{
  double p;
  x=x+6;
  p=1/(x*x);
  p=(((0.004166666666667*p-0.003968253986254)*p+
    0.008333333333333)*p-0.083333333333333)*p;
  p=p+log(x)-0.5/x-1/(x-1)-1/(x-2)-1/(x-3)-1/(x-4)-1/(x-5)-1/(x-6);
  return p;
}


// [[Rcpp::export]]
double lowerbound(int Q, mat& tau, rowvec& n, mat& eta, mat& zeta, int directed) {
  
  double l = - sum(sum(tau % log(tau))) + lgamma(Q) - lgamma(sum(n)) - sum(lgamma(ones<rowvec>(Q)) - lgamma(n));
  
  mat A0 = lgamma(2*ones<mat>(Q,Q));
  mat A = lgamma(eta + zeta);
  mat B0 = lgamma(ones<mat>(Q,Q));
  mat B = lgamma(eta);
  mat C0 = lgamma(ones<mat>(Q,Q));  
  mat C = lgamma(zeta);
  
  if(!directed) {
    l += sum(sum(trimatl(A0) - trimatl(A) - trimatl(B0) + trimatl(B) - trimatl(C0) + trimatl(C)));
  } else {
    l += sum(sum( A0 - A - B0 + B - C0 + C));
  }

  return(l);
  
}

// [[Rcpp::export]]
int VMstep(sp_mat& X, int N, int Q, mat& tau, rowvec& n, mat& eta, mat& zeta, int directed) {

  n = sum(tau, 0);
  
  eta = tau.t()*X*tau;
  
  zeta = n.t()*n - tau.t()*tau - eta;
  
  if(!directed) { 
    eta.diag() = eta.diag()/2;
    zeta.diag() = zeta.diag()/2;
  }
  
  n += 1;
  eta += 1;
  zeta += 1;
  
  return(1);
}

// [[Rcpp::export]]
int VEstep(sp_mat& X, sp_mat& tX, int N, int Q, mat& tau, rowvec& n, mat& eta, mat& zeta, double mincut, double maxcut, int directed) {
  
  // init : prepare elements: start
  int i, k, l;
  double cst;
  
  mat MAXCUT(N, Q);
  MAXCUT.fill(maxcut);
  
  mat MINCUT(N, Q);
  MINCUT.fill(mincut);
  
  mat MIN(N, Q);
  MIN.fill(DBL_MIN);
  
  
  
  rowvec ElogIP(Q);
  for(k=0; k<Q; k++) {
    ElogIP(k) = digamma(n(k)) - digamma((double)(N+Q));
  }
  
  mat EMUbarMU(Q, Q);
  mat EMUbarMUt(Q, Q);
  mat EbarMU(Q, Q);
  mat EbarMUt(Q, Q);
  
  for(k=0; k<Q; k++) {
    for(l=0; l<Q; l++) {
      EMUbarMU(k, l) = digamma(eta(k, l)) - digamma(zeta(k, l)); 
      EbarMU(k, l) = digamma(zeta(k, l)) - digamma(eta(k, l)+zeta(k, l));
    }
  }
  
   EMUbarMUt = EMUbarMU.t();
   EbarMUt = EbarMU.t();
  
  
  if(directed) {
    cst = 1;
  } else {
    cst = 0.5;
  }
  // init : prepare elements: end
  
  rowvec nsize = sum(tau, 0);
  
  mat logTau(N, Q);
  for(i=0; i<N; i++) {
    rowvec tmp(Q);
    tmp.zeros();
    for(l=0; l<Q; l++) {
      tmp += cst*(nsize(l)-tau(i, l))*(EbarMUt.row(l) + EbarMU.row(l)); 
    }
   logTau.row(i) = tmp + ElogIP;
  }
  logTau += cst*(X*tau*EMUbarMUt + tX*tau*EMUbarMU);
  
  logTau = min( logTau, MAXCUT);
  logTau = max( logTau, MINCUT);
  tau = exp( logTau );
  cout << "exp(logTau.row(0)) : " << exp(logTau.row(0)) << endl;
  cout << "exp(logTau.row(0) - max(logTau.row(0))) : " << exp(logTau.row(0) - max(logTau.row(0))) << endl;
  for(i=0; i<N; i++) {
    tau.row(i) = exp(logTau.row(i) - max(logTau.row(i)))/ sum(exp(logTau.row(i) - max(logTau.row(i))));
  }
  
  tau = max( tau, MIN ); 
  cout << "zeta : " << zeta << endl;
  cout << "eta : " << eta << endl;
  //cout << "MIN : " << MIN << endl;
  
  return(1);
  
} 

// [[Rcpp::export]]
mat call_test(sp_mat& X, int Q, mat& tau, vec& n, mat& eta, mat& zeta, double mincut, double maxcut, int max_outer, double eps_outer, int directed) {
  
  tau(0, 0) = 3;

  mat Y;
  Y = X;
  return(Y);
}

// [[Rcpp::export]]
mat VBEM(sp_mat X, int Q, mat &tau, rowvec n, mat eta, mat zeta, double mincut, double maxcut, int max_outer, double eps_outer, int directed) {
  int N = X.n_rows, ite;
  sp_mat tX;

  tX = X.t();
  mat l(max_outer, 1);
  
  for(ite=1; ite<=max_outer; ite++) {

   // cout << "it1 : " << ite << endl;
    VMstep(X, N, Q, tau, n, eta, zeta,directed);
    
    l(ite-1, 0)=lowerbound(Q, tau, n, eta, zeta, directed);
      
    VEstep(X, tX, N, Q, tau, n, eta, zeta, mincut, maxcut, directed);
    
    if(ite==1) {
     // cout << "it1 : " << ite << " lower bound : " << l(ite-1, 0) << endl;
      
    } else {
      double diff=(l(ite-1, 0) - l(ite-2, 0))/l(ite-2, 0);
    //  cout << "ite : " << ite << " lower bound : " << l(ite-1, 0) << " diff : " << -diff << endl;
      if(-diff < eps_outer)  {
        break;
      }
    }
      
  }
  
  if(ite>max_outer) {
    ite = max_outer;
  }
  
 return(l.submat(0, 0, ite-1, 0)); 
}
