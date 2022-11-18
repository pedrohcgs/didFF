#' Test if Parallel trends assumption is sensitive to functional form
#'
#' @param data The name of the data.frame that contains the data
#' @param yname The name of the outcome variable
#' @param tname The name of the column containing the time periods
#' @param idname The individual (cross-sectional unit) id name
#' @param gname The name of the variable in `data` that
#'  contains the first period when a particular observation is treated.
#'  This should be a positive number for all observations in treated groups.
#'  It defines which "group" a unit belongs to.  It can be 0 for units
#'  in the ``never-treated'' group.
#' @param y_already_discretized Whether the outcome of interest is already
#' discretized. Defaults is `FALSE`. If `y_already_discretized = TRUE`,
#' the variable `gname` should contain all the discretized values of the outcome,
#' for each time period and each cross-sectional id.
#' @param weightsname The name of the column containing the sampling weights.
#'  If not set, all observations have same weight (Default is NULL).
#' @param clustervars A vector of variables names to cluster on.  At most, there
#'  can be two variables (otherwise will throw an error) and one of these
#'  must be the same as idname which allows for clustering at the individual
#'  level. By default, we cluster at individual level.
#' @param est_method the method to compute group-time average treatment effects.
#'  The default is "dr" which uses the doubly robust
#' approach in the `DRDID` package.  Other built-in methods
#' include "ipw" for inverse probability weighting and "reg" for
#' first step regression estimators.
#' @param xformla A formula for the covariates to include in the
#'  model.  It should be of the form `~ X1 + X2`.  Default
#'  is NULL which is equivalent to `xformla=~1`.  This is
#'  used to create a matrix of covariates which is then passed
#'  to the 2x2 DID estimator chosen in `est_method`.
#' @param panel Whether or not the data is a panel dataset.
#'  The panel dataset should be provided in long format -- that
#'  is, where each row corresponds to a unit observed at a
#'  particular point in time.  The default is TRUE.
#'  When `panel=FALSE`, the data is treated
#'  as repeated cross sections.
#' @param allow_unbalanced_panel Whether or not function should
#'  "balance" the panel with respect to time and id.  The default
#'  values if `FALSE` which means that [att_gt()] will drop
#'  all units where data is not observed in all periods.
#'  The advantage of this is that the computations are faster
#'  (sometimes substantially).
#' @param control_group Which units to use the control group.
#'  The default is `control_group = "nevertreated"`, which sets the control group
#'  to be the group of units that never participate in the
#'  treatment.  This group does not change across groups or
#'  time periods.  The other option is to set
#'  `control_group="notyettreated"`.  In this case, the control group
#'  is set to the group of units that have not yet participated
#'  in the treatment in that time period.  This includes all
#'  never treated units, but it includes additional units that
#'  eventually participate in the treatment, but have not
#'  participated yet.
#' @param anticipation The number of time periods before participating
#'  in the treatment where units can anticipate participating in the
#'  treatment and therefore it can affect their untreated potential outcomes
#' @param nbins A scalar indicating the (maximum) number of bins for the support of outcome.
#' Default `nbins=100`. Empty bins are dropped.
#' @param numSims Number of simulation draws to compute p-value for moment inequality
#' test. Default `numSims=100000`.
#' @param seed Starting seed for iDensityTest. Default is seed=0, set seed=NULL for random seed.
#' @param lb_graph Minimun outcome-bin for density estimation. Default lb_graph=NULL
#' @param ub_graph Maximun outcome-bin for density estimation. Default ub_graph=NULL
#' @param aggte_type Which type of (scalar) aggregated treatment effect parameter to compute.
#' Options are "simple", "dynamic", "group", and "calendar". Default is `group`.
#' @param balance_e If set (and if `aggte_type = "dynamic"`), it balances
#'  the sample with respect to event time.  For example, if `balance.e=2`,
#'  it will drop groups that are not exposed to treatment for
#'  at least three periods. (the initial period when `e=0` as well as the
#'  next two periods when `e=1` and the `e=2`).  This ensures that
#'  the composition of groups does not change when event time changes.
#' @param min_e For `aggte_type = "dynamic"`, this is the smallest event time to compute
#'  dynamic effects for.  By default, `min_e = -Inf` so that effects at
#'  all feasible event times are computed.
#' @param max_e For `aggte_type = "dynamic"`, this is the largest event time to compute
#'  dynamic effects for.  By default, `max_e = Inf` so that effects at
#'  all lfeasible event times are computed.
#' @param pl Whether or not to use parallel processing. Default is FALSE.
#' @param cores The number of cores to use for parallel processing.
#'  Only relevant if `pl = TRUE`.Default is `cores = 1`.
#' @return A list object containing the plot of the
#'  implied density under the null, a table with the estimated and
#'  implied densities, and the p-value for H0= Implied Density>=0.
#' @export
#'



didFF <-function(
    data,
    yname,
    tname,
    idname,
    gname,
    y_already_discretized = FALSE,
    weightsname = NULL,
    clustervars = NULL,
    est_method = "dr",
    xformla = NULL,
    panel = TRUE,
    allow_unbalanced_panel = FALSE,
    control_group = c("nevertreated","notyettreated"),
    anticipation = 0,
    nbins = 100,
    numSims = 100000,
    seed = 0,
    lb_graph = NULL,
    ub_graph = NULL,
    aggte_type = "group",
    balance_e = NULL,
    min_e = -Inf,
    max_e = Inf,
    pl = FALSE,
    cores=1
){
  #----------------------------------------------------------------------------
  # Store data as DF
  DF <- as.data.frame(data)
  #----------------------------------------------------------------------------
  # Some sanity checks
  if(base::is.null(DF[[yname]])){
    stop("Must provide yname that is part of data.")
  }
  if(base::is.null(DF[[tname]])){
    stop("Must provide tname that is part of data.")
  }
  if(base::is.null(DF[[idname]])){
    stop("Must provide idname that is part of data.")
  }
  if(base::is.null(DF[[gname]])){
    stop("Must provide gname that is part of data.")
  }
  possible_aggte_types <- c("simple", "dynamic", "group", "calendar")
  if((aggte_type %in% possible_aggte_types) == FALSE){
    stop("aggte_type must be equal to `simple`, `dynamic`, `group`, or `calendar`.")
  }

  #----------------------------------------------------------------------------
  # Store never-treated as Infinity
  DF[[gname]] <- base::ifelse(DF[[gname]]==0,
                              Inf,
                              DF[[gname]])
  #----------------------------------------------------------------------------
  # Form bins (regardless of treatment group)
  if(y_already_discretized == FALSE){
    # First do some sanity checks
    if((base::is.null(nbins) | (nbins < 2))) {
      stop("Must provide nbins at least equal to 2")
    }
    #  Build the bins
    bin <- base::as.numeric(base::cut(DF[[yname]],
                                      breaks = nbins,
                                      include.lowest = TRUE,
                                      labels =FALSE))

    level_bin <- base::as.numeric(base::sub("[^,]*,([^]]*)\\]",
                                            "\\1",
                                            base::cut(
                                              DF[[yname]],
                                              breaks = nbins,
                                              include.lowest = TRUE
                                            )
    ) )
    #----------------------------------------------------------------------------
    # get ready for looping
    unique_bin <- base::sort(base::unique(bin))
    unique_level <- base::sort(base::unique(level_bin))
    n_unique_bin <- base::length(unique_bin)
    n_ids <- base::length(base::unique(DF[[idname]]))
  }
  #----------------------------------------------------------------------------
  # Do the same but when y_already_discretized == TRUE
  if(y_already_discretized == TRUE){
    #----------------------------------------------------------------------------
    # get ready for looping
    unique_level <- base::sort(base::unique(DF[[yname]]))
    n_unique_bin <- base::length(unique_level)
    n_ids <- base::length(base::unique(DF[[idname]]))
    bin <- DF[[yname]]
    unique_bin <- unique_level
  }
  #----------------------------------------------------------------------------
  # Initialize matrix of influence functions
  unc_inf_function <- base::matrix(NA,
                                   nrow = n_ids,
                                   ncol = n_unique_bin)

  DF$outcome_bin <- NA
  #----------------------------------------------------------------------------
  # Compute aggte from did package using each bin as outcome
  for (j in 1:n_unique_bin){
    DF$outcome_bin <- -(bin == unique_bin[j])
    DF$outcome_bin <- base::ifelse(
      DF[[gname]] <= DF[[tname]],
      0,
      DF$outcome_bin
    )

    #DF_binned <- DF

    # if(y_already_discretized == TRUE){
    #   if(is.null(weightsname)) {
    #     w <- 1
    #   } else {
    #     w <- DF[[weightsname]]
    #   }
    #
    #   DF_binned <- DF %>%
    #     group_by(idname, tname, outcome_bin)  %>%
    #     summarise()
    #
    #
    # }

    out_bins <- base::suppressMessages(
      did::att_gt(
        yname = "outcome_bin",
        gname = gname,
        idname = idname,
        tname = tname,
        data = DF,
        control_group = control_group,
        xformla = xformla,
        est_method = est_method,
        clustervars = clustervars,
        weightsname = weightsname,
        panel = panel,
        allow_unbalanced_panel = allow_unbalanced_panel,
        anticipation = anticipation,
        bstrap = FALSE,
        cband = FALSE,
        base_period = "universal"
      )
    )

    aggt_param <- base::suppressMessages(
        did::aggte(out_bins,
                   type = aggte_type,
                   na.rm = TRUE,
                   cband = FALSE,
                   bstrap = TRUE,
                   clustervars = clustervars,
                   balance_e = NULL,
                   min_e = -Inf,
                   max_e = Inf
        )
      )


        if(aggte_type=="simple"){
          unc_inf_function[,j] <- aggt_param$overall.att + aggt_param$inf.function$simple.att
        }

        if(aggte_type=="group"){
          unc_inf_function[,j] <-  aggt_param$overall.att + aggt_param$inf.function$selective.inf.func
        }

        if(aggte_type=="dynamic"){
          unc_inf_function[,j] <-  aggt_param$overall.att + aggt_param$inf.function$dynamic.inf.func
        }

        if(aggte_type=="calendar"){
          unc_inf_function[,j] <- aggt_param$overall.att + aggt_param$inf.function$calendar.inf.func
        }
  }
  #----------------------------------------------------------------------------
  # Drop collinear bins (only for testing purposes)
  qr.IF <-  base::qr(unc_inf_function,
                     tol=1e-6,
                     LAPACK = FALSE)
  rnk_IF <- qr.IF$rank
  keep_IF <- qr.IF$pivot[seq_len(rnk_IF)]

  # Do the drops and adjust the other variables
  unc_inf_function <- unc_inf_function[,keep_IF]
  unique_bin <- unique_bin[keep_IF]
  unique_level <- unique_level[keep_IF]
  n_unique_bin <- base::length(unique_bin)
  #----------------------------------------------------------------------------
  # Compute the implied pdf for each bin
  implied_density <- base::colMeans(unc_inf_function)
  #Asymptotic Variance-covariance matrix
  AsyVar <- base::crossprod(unc_inf_function -
                              base::matrix(base::rep(implied_density, n_ids),
                                           ncol = n_unique_bin,
                                           nrow = n_ids,
                                           byrow = TRUE))/n_ids

  # Scale it to account for root-n in AsyVar
  Sigmahat <- (AsyVar/n_ids)
  #----------------------------------------------------------------------------
  # get the implied density table
  level = NULL
  implied_density_table <- base::data.frame(
    level = unique_level,
    implied_density = implied_density
  )
  #----------------------------------------------------------------------------
  # Prepare data for plots
  if(base::is.null(lb_graph)){
    lb_graph <- base::min(implied_density_table$level)
  }

  if(is.null(ub_graph)){
    ub_graph <- base::max(implied_density_table$level)
  }
  Outcome = NULL

  plotTable <- implied_density_table %>%
    dplyr::filter(lb_graph <= level, level <= ub_graph) %>%
    dplyr::mutate(Outcome = level) %>%
    dplyr::mutate(
      c = base::ifelse(implied_density < 0,
                       "Negative",
                       "Non-negative"))
  `Implied Density` <- NULL
  basic_plot<- plotTable %>%
    ggplot2::ggplot(ggplot2::aes(x=Outcome,
                                 y = implied_density,
                                 fill = `Implied Density`)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Outcome") +
    ggplot2::ylab("Implied Density")

  #Format basic plot
  implied_density_plot <- basic_plot +
    ggthemes::theme_clean(base_size=12) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::theme(
      # Background
      plot.background = ggplot2::element_blank(),
      # Format legend
      legend.text = ggplot2::element_text(size=10),
      legend.title = ggplot2::element_text(size=10),
      legend.box.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),

      # Set title and axis labels, and format these and tick marks
      plot.title=ggplot2::element_text(size=13, vjust=1.25, hjust = 0.5),
      axis.text.x=ggplot2::element_text(size=10),
      axis.text.y=ggplot2::element_text(size=10),
      axis.title.x=ggplot2::element_text(size=10, vjust=0),
      axis.title.y=ggplot2::element_text(size=10, vjust=1.25),

      # Plot margins
      plot.margin = grid::unit(c(0.35, 0.2, 0.3, 0.35), "cm")
    )
  #----------------------------------------------------------------------------
  # Now, compute the moment inequality test
  # Get correlation matrix implied by sigmahat
  Cormat <- stats::cov2cor(Sigmahat)

  # set seed for draws
  if (is.null(seed)) {
    s <- Sys.time()
  } else {
    s <- seed
  }
  set.seed(s)

  muhat_test <- - implied_density
  sims <- MASS::mvrnorm(n = numSims,
                        mu = 0*muhat_test,
                        Sigma = Cormat
  )

  sims_max <- base::apply(X = sims, MARGIN = 1, FUN = max)

  p_value <- mean( sims_max >= max(muhat_test/sqrt(diag(Sigmahat))) )


  #rpval <- round(p_value,3)

  #H0_text = list("H[0]: 'Implied Density' >= 0")

  # if(p_value < 0.01){
  #   pval_text= list("p-value <0.01")
  # } else{
  #   pval_text=list(paste("p-value =",rpval))
  # }


  #
  #   plot <- implied_density_plot +
  #     ggplot2::annotate(geom = 'text',
  #                       x = base::mean(plotTable$level)+
  #                         stats::sd(plotTable$implied_density),
  #                       y = base::max(plotTable$implied_density),
  #                       label = H0_text, parse=TRUE,
  #                       hjust = 0) +
  #     ggplot2::annotate(geom = 'text',
  #                       x = base::mean(plotTable$level)+
  #                         stats::sd(implied_density_table$implied_density),
  #                       y = base::max(plotTable$implied_density)-
  #                         stats::sd(plotTable$implied_density)/3,
  #                       label = pval_text,
  #                       hjust = 0) +
  #     ggplot2::xlab(yvar)

  plot <- implied_density_plot

  didFF_out <- list(
    plot = plot,
    table = implied_density_table,
    pval = p_value
  )

  return(didFF_out)


}

