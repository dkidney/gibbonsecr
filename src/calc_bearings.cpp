// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp ;

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// @export
// [[Rcpp::export]]
NumericMatrix calc_bearings_rcpp(const NumericMatrix& A, const NumericMatrix& Z){

    NumericMatrix bearings(Z.nrow(), A.nrow()) ;

    double opp, adj  ;

    for (int k = 0 ; k < A.nrow() ; k++) {

        for (int m = 0 ; m < Z.nrow() ; m++) {

            opp = Z(m,0) - A(k,0) ;

            adj = Z(m,1) - A(k,1) ;

            bearings(m,k) = atan(opp/adj) ;

            if(ISNAN(bearings(m,k))){

                bearings(m,k) = 0 ;

            }else{

                if(adj < 0) bearings(m,k) += M_PI ;

                if((opp < 0) & (adj >= 0)) bearings(m,k) += M_2PI ;

            }

        }

    }

    return bearings ;

}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
