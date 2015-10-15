// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp ;

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// calculate nearest point in B to all points in A

// [[Rcpp::export]]
NumericVector nearest_rcpp(const NumericMatrix& A, const NumericMatrix& B){
    NumericVector nearest(A.nrow()) ;
    NumericVector distances(B.nrow()) ;
    for (int i = 0 ; i < A.nrow() ; i++) {
        for (int j = 0 ; j < B.nrow() ; j++) {
            distances(j) = pow(pow(A(i,0) - B(j,0),2) + pow(A(i,1) - B(j,1), 2), 0.5) ;
        }
        nearest(i) = which_min(distances) + 1 ;
    }
    return nearest ;
}

// A = as.matrix(expand.grid(x = c(0,10), y = c(0,10))) ; dim(A)
// plot(A, pch = 19, col = 4)
// B = as.matrix(expand.grid(x = 1:9, y = 1:9)) ; dim(B)
// points(B, pch = 19, col = 3)
// i = nearest_rcpp(A, B)
// C = B[i,]
// points(C, pch = 19, col = 2)

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
