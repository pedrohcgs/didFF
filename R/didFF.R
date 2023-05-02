#' Test if Parallel trends assumption is sensitive to functional form
#'
#' @param data The name of the data.frame that contains the data
#' @param yname The name of the outcome variable
#' @param tname The name of the column containing the time periods
#' @param idname The cross-sectional unit id name
#' @param gname The name of the variable in `data` that
#'  contains the first period when a particular observation is treated.
#'  This should be a positive number for all observations in treated groups.
#'  It defines which "group" a unit belongs to.  It can be `0`  or `Inf` for units
#'  in the ``never-treated'' group.
#' @param weightsname The name of the column containing the sampling weights.
#'  If not set, all observations have the same weight (Default is NULL).
# @param clustervars A vector of variables names to cluster on.  At most, there
#  can be two variables (otherwise will throw an error) and one of these
#  must be the same as idname which allows for clustering at the unit
#  level. By default, we cluster at unit level.
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
#'   Default `nbins=20`. Empty bins are dropped.
#' @param binpoints Alternative to nbins: A vector indicating the interval endpoints to use;
#'   if the data range is not included then `min(y)` and `max(y)` are added as endpoints. For
#'   a user-specified vector `a = c(a_1, a_2, ..., a_n)`, let `b = a` if `min(y) >= min(a)`
#'   and `b = c(min(y), a)` otherwise; then let `c = b` if `max(y) <= max(a)` and
#'   `c = c(b, max(y))` otherwise. Bins are `[c_1, c_2]`, `(c_2, c_3]`, ..., `(c_{n-1}, c_n]`.
#'   Empty bins are dropped.
#'   Default is NULL (`nbins` is used). Empty bins are dropped.
#' @param numSims Number of simulation draws to compute p-value for moment inequality
#' test. Default `numSims=100000`.
#' @param seed Starting seed for moment inequality test. Default is seed=0, set seed=NULL for random seed.
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
#'  Only relevant if `pl = TRUE`.Default is `cores = parallel::detectCores()`.
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
    weightsname            = NULL,
    #clustervars           = NULL,
    est_method             = "dr",
    xformla                = NULL,
    panel                  = TRUE,
    allow_unbalanced_panel = FALSE,
    control_group          = base::c("nevertreated","notyettreated"),
    anticipation           = 0,
    nbins                  = NULL,
    binpoints              = NULL,
    numSims                = 100000,
    seed                   = 0,
    #lb_graph              = NULL,
    #ub_graph              = NULL,
    aggte_type             = "group",
    balance_e              = NULL,
    min_e                  = -Inf,
    max_e                  = Inf,
    pl                     = FALSE,
    cores                  = parallel::detectCores()
) {
  if ( !exists(".Random.seed") ) stats::runif(1)
  rseed.cached <- .Random.seed
  base::on.exit({.Random.seed <<- rseed.cached})
  #----------------------------------------------------------------------------
  # Store data as DF
  DF <- as.data.frame(data)
  #----------------------------------------------------------------------------
  # Some sanity checks
  if(base::is.null(DF[[yname]])){
    base::stop("Must provide yname that is part of data.")
  }
  if(base::is.null(DF[[tname]])){
    base::stop("Must provide tname that is part of data.")
  }
  if(base::is.null(DF[[idname]])){
    base::stop("Must provide idname that is part of data.")
  }
  if(base::is.null(DF[[gname]])){
    base::stop("Must provide gname that is part of data.")
  }
  if(!base::is.null(nbins) & !base::is.null(binpoints)){
    base::stop("Can only specify one of nbins or binpoints.")
  }
  possible_aggte_types <- c("simple", "dynamic", "group", "calendar")
  if((aggte_type %in% possible_aggte_types) == FALSE){
    base::stop("aggte_type must be equal to `simple`, `dynamic`, `group`, or `calendar`.")
  }
  #----------------------------------------------------------------------------
  # Store never-treated as Infinity
  DF[[gname]] <- base::ifelse(DF[[gname]]==0,
                              Inf,
                              DF[[gname]])
  #----------------------------------------------------------------------------
  # Form bins (regardless of treatment group)

  # First do some sanity checks
  binsel <- DF[[tname]] < DF[[gname]]
  if(base::is.null(nbins) & base::is.null(binpoints)){
    nbins <- 20
  }
  if( !base::is.null(nbins) && (nbins < 2) ){
    base::stop("Must provide nbins at least equal to 2")
  }
  if( !base::is.null(binpoints) ){
    if( base::length(binpoints) < 1 ){
      base::stop("Must provide at least 1 binpoints")
    }
    binpoints <- base::sort(binpoints)
    yrange <- base::range(DF[binsel, yname])
    if( binpoints[1] > yrange[1] ){
      binpoints <- base::c(yrange[1], binpoints)
    }
    if( binpoints[base::length(binpoints)] < yrange[2] ){
      binpoints <- base::c(binpoints, yrange[2])
    }
    if( (binpoints[1] < yrange[1]) & (yrange[2] < binpoints[base::length(binpoints)]) ){
      base::stop("Provided binpoints group all values into single bin.")
    }
  }

  #  Build the bins
  bin <- base::rep(NA, base::NROW(DF))
  bins <- base::cut(DF[binsel, yname],
                    breaks = if (base::is.null(nbins)) binpoints else nbins,
                    include.lowest = TRUE,
                    labels = NULL)
  bin[binsel] <- base::as.numeric(bins)
  bin2 <- base::levels(base::droplevels(bins))
  level_bin <- base::as.numeric(base::sub("[^,]*,([^]]*)\\]", "\\1", bins))

  #----------------------------------------------------------------------------
  # get ready for looping
  unique_bin   <- base::sort(base::unique(bin))
  unique_level <- base::sort(base::unique(level_bin))
  n_unique_bin <- base::length(unique_bin)
  # Number of ids in the data
  n_ids <- base::length(base::unique(DF[[idname]]))
  # We adjust this later for case of unbalanced panel and RCS
  # bc for these case, we do not use all ids (as we set some outcomes to 0)

  #----------------------------------------------------------------------------

  #----------------------------------------------------------------------------
  # Initialize matrix of influence functions
  inf_function <- base::matrix(NA,
                               nrow = n_ids,
                               ncol = n_unique_bin)
  point_estimates <- base::rep(NA, times = n_unique_bin)

  # Ensure never treated is coded as Inf
  DF[[gname]] <- base::ifelse(
    DF[[gname]]==0,
    Inf,
    DF[[gname]]
  )

  #----------------------------------------------------------------------------
  # Compute aggte from did package using each bin as outcome
  run_bin <- function(j){
    outname <- base::paste0("outcome_bin", as.character(j))

    DF[[outname]] <- -(bin == unique_bin[j])
    DF[[outname]] <- base::ifelse(DF[[gname]] <= DF[[tname]], 0, DF[[outname]])
    out_bins <- base::suppressMessages(
      base::suppressWarnings(
        did::att_gt(
          yname                  = outname,
          gname                  = gname,
          idname                 = idname,
          tname                  = tname,
          data                   = DF,
          control_group          = control_group,
          xformla                = xformla,
          est_method             = est_method,
          clustervars            = NULL,
          weightsname            = weightsname,
          panel                  = panel,
          allow_unbalanced_panel = allow_unbalanced_panel,
          anticipation           = anticipation,
          bstrap                 = FALSE,
          cband                  = FALSE,
          base_period            = "universal"
        )
      )
    )
    DF[[outname]] <- NULL

    aggt_param <- base::suppressMessages(
      did::aggte(out_bins,
                 type        = aggte_type,
                 na.rm       = TRUE,
                 cband       = FALSE,
                 bstrap      = FALSE,
                 clustervars = NULL,
                 balance_e   = balance_e,
                 min_e       = min_e,
                 max_e       = max_e
      )
    )
    return(aggt_param)
  }

  if(aggte_type=="simple"){
    get_inf <- function(aggt_param) {aggt_param$inf.function$simple.att}
  }
  if(aggte_type=="group"){
    get_inf <- function(aggt_param) {aggt_param$inf.function$selective.inf.func}
  }
  if(aggte_type=="dynamic"){
    get_inf <- function(aggt_param) {aggt_param$inf.function$dynamic.inf.func}
  }
  if(aggte_type=="calendar"){
    get_inf <- function(aggt_param) {aggt_param$inf.function$calendar.inf.func}
  }

  if( pl ){
    cl <- parallel::makeCluster(cores)
    parallel::clusterExport(cl, c("DF",
                                  "bin",
                                  "unique_bin",
                                  "gname",
                                  "tname",
                                  "idname",
                                  "control_group",
                                  "xformla",
                                  "est_method",
                                  "weightsname",
                                  "panel",
                                  "allow_unbalanced_panel",
                                  "anticipation",
                                  "balance_e",
                                  "min_e",
                                  "max_e",
                                  "aggte_type"),
                            envir=environment())
    results <- parallel::parLapply(cl, 1:n_unique_bin, run_bin)
    # doParallel::registerDoParallel(cl)
    # results <- foreach::foreach(j=1:n_unique_bin) %dopar% run_bin(j)
    parallel::stopCluster(cl)
  } else {
    results <- base::lapply(1:n_unique_bin, run_bin)
  }
  point_estimates <- base::unlist(base::lapply(results, function(x) x$overall.att))
  inf_function    <- base::do.call(base::cbind, base::lapply(results, get_inf))

  #----------------------------------------------------------------------------
  # Drop collinear bins (only for hypothesis testing purposes) - deprecated
  # qr.IF <- base::qr(inf_function,
  #                   tol=1e-6,
  #                   LAPACK = FALSE)
  # rnk_IF  <- qr.IF$rank
  # keep_IF <- qr.IF$pivot[base::seq_len(rnk_IF)]

  # Drop bins with zero (or very close to zero) variance
  keep_IF <- base::colSums((inf_function[])^2)>1e-6

  # Do the drops and adjust the other variables
  inf_function      <- inf_function[,keep_IF,drop=FALSE]
  unique_bin        <- unique_bin[keep_IF]
  unique_level_orig <- unique_level
  unique_level      <- unique_level[keep_IF]
  n_unique_bin      <- base::length(unique_bin)

  # Drop all columns with all 0's (id was not used in the test)
  inf_function2 <- inf_function[base::rowSums(base::abs(inf_function[]))>1e-6,,drop=FALSE]
  n_ids_eff <- base::NROW(inf_function2)

  #----------------------------------------------------------------------------
  # Compute the implied pdf for each bin
  implied_density <- point_estimates[keep_IF]

  #Asymptotic Variance-covariance matrix
  AsyVar <- base::crossprod(inf_function)/n_ids_eff

  # Scale it to account for root-n in AsyVar
  Sigmahat <- (AsyVar/n_ids_eff)

  #----------------------------------------------------------------------------
  # get the implied density table
  level = NULL
  implied_density_table <- base::data.frame(
    level = unique_level_orig,
    #level = bin2,
    implied_density = point_estimates
  )

  #----------------------------------------------------------------------------
  # Prepare data for plots
  lb_graph <- base::min(unique_level_orig)
  ub_graph <- base::max(unique_level_orig)

  #Outcome = NULL
  plotTable <- implied_density_table %>%
    #dplyr::filter(lb_graph <= level, level <= ub_graph) %>%
    dplyr::mutate(Outcome = level) %>%
    dplyr::mutate(
      `Implied Density` = base::ifelse(implied_density < 0,
                                       "Negative",
                                       "Non-negative"))
  #`Implied Density` <- NULL
  basic_plot<- plotTable %>%
    ggplot2::ggplot(ggplot2::aes(x=Outcome,
                                 y = implied_density,
                                 # fill = c)) +
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
  if (base::is.null(seed)) {
    s <- base::Sys.time()
  } else {
    s <- seed
  }
  base::set.seed(s)

  muhat_test <- -implied_density
  muhat_max  <- base::max(muhat_test/base::sqrt(base::diag(Sigmahat)))
  sims       <- MASS::mvrnorm(n = numSims, mu = rep(0, n_unique_bin), Sigma = Cormat)
  sims_max   <- base::apply(X = sims, MARGIN = 1, FUN = max)
  p_value    <- base::mean( sims_max >=  muhat_max)
  plot       <- implied_density_plot

  # To have the outcome interval in the table of implied density
  implied_density_table$level <- bin2

  didFF_out <- list(
    plot  = plot,
    table = implied_density_table,
    pval  = p_value
  )

  return(didFF_out)
}
