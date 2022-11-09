lf_moment_inequality_test <-
  function(muhat,
           Sigmahat,
           numSims = 10^5,
           seed = NULL){
    if (is.null(seed)) {
      s<-Sys.time()
    } else {
      s<-seed
    }
    set.seed(s)

    Cormat <- stats::cov2cor(Sigmahat)

    sims <- MASS::mvrnorm(n=numSims,
                          mu= 0*muhat,
                          Sigma = Cormat
    )

    sims_max <- base::apply(X = sims, MARGIN = 1, FUN = max)

    p_value <- mean( sims_max >= max(muhat/sqrt(diag(Sigmahat))) )

    return(p_value)
  }
