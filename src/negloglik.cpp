// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h> 
using namespace Rcpp ;
#include "negloglik.h"

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// @title
// Evaluate the negative likelihood for data from a single array
//
// @description
// This function is written using Rcpp function as is called by a wrapper function (\code{gibbonsecr_negloglik_wrapper}
// (which is called by \code{gibbonsecr_fit} when \code{locations = FALSE}).
// 
// @details
// All inputs should be for a single array.
//
// @param data survey data 
// @param mask mask object
// @param pars list of paramter values
// @param detected TODO
// @param usage TODO
// @param n number of detected groups 
// @param S number of sampling occasions
// @param K number of traps
// @param M number of mask points
// @param a area of each mask point
// @param detectfn_code TODO
// @param bear_code TODO
// @param distances_pdf_code TODO
// @author Darren Kidney \email{darrenkidney@@googlemail.com}
// @examples
// # Examples:
// [[Rcpp::export]]
double negloglik_rcpp(
    const List& data,
    const List& mask,
    const List& pars,
    const IntegerMatrix& detected,
    const IntegerMatrix& usage,
    int n,
    int S,
    int K,
    
    int M,
    double a,
    int detectfn_code,
    int bearings_pdf_code,
    int distances_pdf_code){

    // ---------------------------------------------------------------------- //
    //                                 inputs                                 //
    // ---------------------------------------------------------------------- //

    // process generic inputs
    IntegerVector capthist_       = data["capthist"]  ; 
    NumericVector dist_mask_ = mask["distances"] ; 
    NumericVector D_              = pars["D"]         ; 
    NumericVector g0_             = pars["g0"]        ; 
    NumericVector sigma_          = pars["sigma"]     ; 
    NumericVector pcall_          = pars["pcall"]    ; 
    NumericVector z_ ; switch(detectfn_code){ 
        case 0: z_ = NA_REAL   ; break ; // set to NA if half normal
        case 1: z_ = pars["z"] ; break ; // set to z if hazard rate
    }

    // convert Rcpp classes into Armadillo classes
    arma::icube capthist(      capthist_.begin(),       n, S, K, false) ;
    arma::vec   D(             D_.begin(),              M,       false) ; 
    arma::mat   g0(            g0_.begin(),             S, K,    false) ; 
    arma::mat   sigma(         sigma_.begin(),          S, K,    false) ; 
    arma::mat   z(             z_.begin(),              S, K,    false) ; 
    arma::vec   pcall(         pcall_.begin(),          S,       false) ; 
    arma::mat   dist_mask(dist_mask_.begin(), M, K,    false) ; 

    // declare new objects
    arma::mat  loglik            = arma::zeros<arma::mat> (M,n  ) ;
    double constant, sumK = 0, sumSK = 0, sumM_Dpdot = 0 ; 
    

    // ---------------------------------------------------------------------- //
    //                         detection probabilities                       //
    // ---------------------------------------------------------------------- //

    // detetction probabilities for each maskpoint-occasion-trap combination
    detectfn_pointer detectfn = get_detectfn(detectfn_code) ;
    arma::cube detprob = arma::zeros<arma::cube>(M,S,K) ;
    for (int k=0 ; k<K ; k++) {
        for (int s=0 ; s<S ; s++) {
            if (usage(k,s) == 1) {
                switch(detectfn_code){ 
                    case 0: constant = -0.5 * pow(sigma(s,k), -2) ; break ; 
                    case 1: constant = -pow(sigma(s,k), z(s,k)) ; break ; 
                } ; 
                for (int m=0 ; m<M ; m++) {
                    detprob(m,s,k) = detectfn(dist_mask(m,k), g0(s,k), sigma(s,k), z(s,k), constant) ;
                }
            }
        }
    }

    // arma::cube detprob = calc_detprob_rcpp(g0, sigma, z, dist_mask, usage, M, S, K, detectfn_code) ;
    
    // ---------------------------------------------------------------------- //
    //                                  pdot                                  //
    // ---------------------------------------------------------------------- //

    arma::cube one_minus_detprob = arma::ones <arma::cube>(M,S,K) ;
    arma::mat  prodK             = arma::ones <arma::mat> (M,S  ) ;
    arma::vec  prodSK            = arma::ones <arma::vec> (M    ) ;

    // non-detection probabilities for each maskpoint-occasion combination
    // arma::cube one_minus_detprob = 1.0 - detprob ;
    one_minus_detprob = 1.0 - detprob ;
    for (int k=0 ; k<K ; k++) {
        for (int s=0 ; s<S ; s++) {
            for (int m=0 ; m<M ; m++) {
                    prodK(m,s) *= one_minus_detprob(m,s,k) ; 
            }
        }
    }

    // non-detection probabilities for each maskpoint
    // see DK thesis (Sec 6.2, p104) for an explanation of the pcall bit
    for (int s=0 ; s<S ; s++) {
        if(pcall(s) < 1){
            for (int m=0 ; m<M ; m++) prodSK(m) *= (1.0 - pcall(s)) + pcall(s) * prodK(m,s) ; 
        }else{
            for (int m=0 ; m<M ; m++) prodSK(m) *= prodK(m,s) ; 
        }
    }
    
    // detection probabilities for each maskpoint    
    arma::vec pdot = 1.0 - prodSK ;

    // arma::vec pdot = calc_pdot_rcpp(one_minus_detprob, pcall, M, S, K) ;

    // sum of density multiplied by detection probability at each maskpoint    
    for (int m=0 ; m<M ; m++) sumM_Dpdot += D(m) * pdot(m) ; 


    // ---------------------------------------------------------------------- //
    //                             capture histories                          //
    // ---------------------------------------------------------------------- //

    for(int i=0 ; i<n ; i++){    
        for(int m=0 ; m<M ; m++){    
            sumSK = 0 ; 
            for(int s=0 ; s<S ; s++){ 
                sumK = 0 ; 
                for(int k=0 ; k<K ; k++){    
                    switch(capthist(i,s,k)){
                        case 1 : if(detprob(m,s,k) != 1) sumK += log(detprob(m,s,k)) ; break ;
                        case 0 : if(detprob(m,s,k) != 0) sumK += log(one_minus_detprob(m,s,k)) ; break ;
                    }
                }
                if(pcall(s) < 1){
                    switch(detected(i,s)){
                        case 0: sumSK += log( 1.0 - pcall(s) + pcall(s) * exp(sumK) ) ; break ;
                        case 1: sumSK += log(pcall(s)) + sumK ; break ;
                    }
                }else{
                    sumSK += sumK ;
                }
            }
            loglik(m,i) = log(D(m)) + sumSK ; 
        }
    }


    // ---------------------------------------------------------------------- //
    //                                bearings                                //
    // ---------------------------------------------------------------------- //

    if(bearings_pdf_code != 0){

        NumericVector bear_est_  = data["bearings"] ; 
        NumericVector bear_mask_ = mask["bearings"] ; 
        NumericVector bear_par_  = pars["bearings"] ; 

        arma::cube bear_est (bear_est_.begin(),  n, S, K, false) ;
        arma::mat  bear_mask(bear_mask_.begin(), M, K,    false) ; 
        arma::mat  bear_par (bear_par_.begin(),  S, K,    false) ; 

        pdf_pointer bearings_pdf = get_bearings_pdf(bearings_pdf_code) ;

        for(int k=0 ; k<K ; k++){    
            for(int s=0 ; s<S ; s++){ 
                switch(bearings_pdf_code){
                    case 1: constant = M_LN_2PI + log(R::bessel_i(bear_par(s,k), 0.0, 1.0)) ; break ;
                    case 2: constant = log( 1.0 - pow(bear_par(s,k), 2) ) - M_LN_2PI ; break ;
                }
                for(int i=0 ; i<n ; i++){    
                    if(!ISNA(bear_est(i,s,k))){
                        for(int m=0 ; m<M ; m++){    
                            loglik(m,i) += bearings_pdf(bear_est(i,s,k), bear_mask(m,k), bear_par(s,k), constant) ;
                        }
                    }
                }
            }
        }
    }
    
    // ---------------------------------------------------------------------- //
    //                                distances                               //
    // ---------------------------------------------------------------------- //

    if(distances_pdf_code != 0){

        NumericVector dist_est_ = data["distances"] ; 
        NumericVector dist_par_ = pars["distances"] ;

        arma::cube dist_est(dist_est_.begin(), n, S, K, false) ;
        arma::mat  dist_par(dist_par_.begin(), S, K,    false) ; 

        pdf_pointer distances_pdf = get_distances_pdf(distances_pdf_code) ;

        for(int k=0 ; k<K ; k++){    
            for(int s=0 ; s<S ; s++){ 
                switch(distances_pdf_code){
                    case 1: constant = dist_par(s,k) * log(dist_par(s,k)) - lgamma(dist_par(s,k)) ; break ;
                    case 2: constant = -log(dist_par(s,k)) - M_LN_SQRT_2PI ; break ;
                }
                for(int i=0 ; i<n ; i++){    
                    if(!ISNA(dist_est(i,s,k))){
                        for(int m=0 ; m<M ; m++){    
                            loglik(m,i) += distances_pdf(dist_est(i,s,k), dist_mask(m,k), dist_par(s,k), constant) ;
                        }
                    }
                }
            }
        }
    }

    // ---------------------------------------------------------------------- //
    //         combine conditional likelihood with Poisson model for n        //
    // ---------------------------------------------------------------------- //

    double ll = arma::as_scalar(R::dpois(n, a * sumM_Dpdot, true) - n * log(sumM_Dpdot) + sum(log(sum(exp(loglik), 0)), 1)) ; 

    return -ll ; 

}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
