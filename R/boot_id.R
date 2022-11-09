boot_id<-function(DF=NULL, idvar="id", seed=NULL){
  if (is.null(seed)) {
    s<-Sys.time()
  } else {
    s<-seed
  }
    set.seed(s)
    states <- unique(DF[[idvar]])
    bootstrap_states <- states[ sample.int(n = length(states), replace = TRUE) ]

    statesDF <- data.frame(statenum_bootstrap = 1:length(states),
                           bootstrap_id = bootstrap_states)

    bootstrap_DF <-
      dplyr::left_join(statesDF, DF,
                by = c("bootstrap_id" = idvar)) %>%
      dplyr::rename(id = statenum_bootstrap)

    return(bootstrap_DF)
  }#Compute n out of n bootstrap sampling with replacement
