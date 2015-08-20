// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// Detection function

// detection function pointer
typedef double (*detectfn_pointer)(const double&, const double&, const double&, const double&, const double&) ;

// half normal detection function
// in this case distances entered as squared distances so no need to square x  
double hn(const double& x, const double& g0, const double& sigma, const double& z, const double& constant){
    double prob = g0 * exp(pow(x,2) * constant) ;
    return prob ;
}


// hazard rate detection function
double hr(const double& x, const double& g0, const double& sigma, const double& z, const double& constant){
    double prob = g0 * (1 - exp(pow(x, -z) * constant)) ;
    return prob ;
}

// detection function pointer - points to either hn or hr, depending on detectfn_code
detectfn_pointer get_detectfn(int detectfn_code){
    detectfn_pointer detectfn ;
    switch(detectfn_code){
        case 0: detectfn = hn ; break ;
        case 1: detectfn = hr ; break ;
    }
    return(detectfn) ;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// Log pdf functions for bearing estimates distributions

// pdf function pointer - also works for distances
typedef double (*pdf_pointer)(const double&, const double&, const double&, const double&) ;

// von mises 
double log_dvm(const double& x, const double& EX, const double& kappa, const double& constant){
    double log_density = kappa * cos(x - EX) - constant ;
    return log_density ;
}

// wrapped cauchy 
double log_dwc(const double& x, const double& EX, const double& rho, const double& constant){
    double log_density = constant - log(1 + pow(rho,2) - 2 * rho * cos(x - EX)) ;
    return log_density ;
}

// bearings pdf pointer - points to either log_dvm or log_dwc, depending on bearings_code
pdf_pointer get_bearings_pdf(int bearings_code){
    pdf_pointer bearings_pdf ;
    switch(bearings_code){
        case 1: bearings_pdf = log_dvm ; break ;
        case 2: bearings_pdf = log_dwc ; break ;
    }
    return(bearings_pdf) ;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// Log pdf functions for distance estimates distributions

// gamma
double log_dgamma(const double& x, const double& EX, const double& shape, const double& constant){
    double log_density = constant - shape * log(EX) + (shape - 1) * log(x) - (shape * x) / EX ;
    return log_density ;
}

// log normal
double log_dlnorm(const double& x, const double& EX, const double& sigma, const double& constant){
    double log_density = constant - log(x) - pow(log(x) - (log(EX) - pow(sigma, 2) / 2), 2) / 2 / pow(sigma, 2) ;
    return log_density ;
}

// distances pdf pointer - points to either log_gamma or log_dlnorm, depending on distances_code
pdf_pointer get_distances_pdf(int distances_code){
    pdf_pointer distances_pdf ;
    switch(distances_code){
        case 1: distances_pdf = log_dgamma ; break ;
        case 2: distances_pdf = log_dlnorm ; break ;
    }
    return(distances_pdf) ;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

arma::cube calc_detprob_rcpp(
        const arma::mat& g0, 
        const arma::mat& sigma, 
        const arma::mat& z, 
        const arma::mat& distances,
        const IntegerMatrix& usage,
        int detectfn_code,
        int M, int S, int K){

    arma::cube detprob = arma::zeros<arma::cube>(M,S,K) ;
    detectfn_pointer detectfn = get_detectfn(detectfn_code) ;
    double constant ;
    for (int k=0 ; k<K ; k++){
        for (int s=0 ; s<S ; s++) {
            if (usage(k,s) == 1) {
                switch(detectfn_code){ 
                    case 0: constant = -0.5 * pow(sigma(s,k), -2) ; break ; 
                    case 1: constant = -pow(sigma(s,k), z(s,k)) ; break ; 
                } ; 
                for (int m=0 ; m<M ; m++) {
                    detprob(m,s,k) = detectfn(distances(m,k), g0(s,k), sigma(s,k), z(s,k), constant) ;
                }
            }
        }
    }
    return detprob ;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

arma::vec calc_pdot_rcpp(
        const arma::cube& one_minus_detprob, 
        const arma::vec& pcall, 
        int M, int S, int K){

    arma::mat  prodK  = arma::ones <arma::mat> (M,S  ) ;
    arma::vec  prodSK = arma::ones <arma::vec> (M    ) ;

    // probability of not being detected on a given occasion at a given trap
    for (int k=0 ; k<K ; k++) {
        for (int s=0 ; s<S ; s++) {
            for (int m=0 ; m<M ; m++) {
                    prodK(m,s) *= one_minus_detprob(m,s,k) ; 
            }
        }
    }

    // probability of not being detected on a given occasion at any trap
    // see DK thesis (Sec 6.2, p104) for an explanation of the pcall bit
    for (int s=0 ; s<S ; s++) {
        if(pcall(s) < 1){
            for (int m=0 ; m<M ; m++) prodSK(m) *= (1.0 - pcall(s)) + pcall(s) * prodK(m,s) ; 
        }else{
            for (int m=0 ; m<M ; m++) prodSK(m) *= prodK(m,s) ; 
        }
    }
    
    // probability of being detected
    arma::vec pdot = 1.0 - prodSK ;
    
    return pdot ;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

