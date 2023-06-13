library(testthat)
library(didFF)

#-----------------------------------------------------------------------------
# Base example
start_t = 2003 #pre-treatment year
end_t   = 2004 #post-treatment year
cohort  = 2004

data_filtered = subset(didFF::mw_df,  year == start_t | year == end_t)
data_filtered = subset(data_filtered, first.treat == cohort | first.treat == 0)

# Recode first.treat ==0 to first.treat==Inf
data_filtered$first.treat[data_filtered$first.treat == 0] <- Inf
data_filtered$weights <- sample(1:4, NROW(data_filtered), replace=T)

# Parameters for the function
data                   = data_filtered
yname                  = "lemp"
tname                  = "year"
idname                 = "countyreal"
gname                  = "first.treat"
xformla                = ~1
xformla                = ~ lpop + I(lpop^2)
est_method             = "dr"
control_group          = "nevertreated"
nbins                  = 20
anticipation           = 0
allow_unbalanced_panel = FALSE
panel                  = TRUE
aggte_type             = "group"
weightsname            = NULL
nevertreated           = NULL
binpoints              = NULL
numSims                = 100000
seed                   = 0
balance_e              = NULL
min_e                  = -Inf
max_e                  = Inf
test.option            = TRUE

#######################################################################
#                                                                     #
#                          Copied from didFF                          #
#                                                                     #
#######################################################################

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
if ( base::any(DF[[gname]] == Inf) ) {
if ( base::is.null(nevertreated) ) {
  # Nothing to do
} else if ( nevertreated != Inf ) {
  base::warning(base::paste0("You have requested cohort g=", nevertreated, " be used as never-treated but at least one cohort is already coded as Inf."))
  DF[[gname]] <- base::ifelse(DF[[gname]]==nevertreated,
                              Inf,
                              DF[[gname]])
}
} else {
if ( base::is.null(nevertreated) ) {
  if ( base::any(DF[[gname]] == 0) ) {
    if ( base::min(DF[[tname]]) > 0 ) {
      base::warning("You have observations with g=0 and all time periods t>0; assuming the g=0 cohort is never-treated.")
      DF[[gname]] <- base::ifelse(DF[[gname]]==0,
                                  Inf,
                                  DF[[gname]])
    } else {
      msg <- "You have observations with g=0 and at least one time period t <= 0. We assume this means that cohort's treatment started at time 0; if instead this means those units are never-treated, use option nevertreated=0 instead."
      if ( "nevertreated" %in% control_group ) {
        base::stop(base::paste0("No never-treated cohorts identified: ", msg))
      } else {
        base::warning(msg)
      }
    }
  } else {
    msg <- "No cohorts g=Inf or g=0 found; assuming no cohorts are never-treated. Use option nevertreated to specify the never-treated cohort."
    if ( "nevertreated" %in% control_group ) {
      base::stop(msg)
    } else {
      base::warning(msg)
    }
  }
} else {
  DF[[gname]] <- base::ifelse(DF[[gname]]==nevertreated,
                              Inf,
                              DF[[gname]])
}
}

#----------------------------------------------------------------------------
# Form bins (regardless of treatment group)

# This displays the right warnings from did wrt balancing the data,
# etc.  It also sets up the data for att_gt. This is processed here so
# the bins are created only from this data.
#
# Also need to mask g, t so it's all > 0; currently never treated is
# coded as Inf or there are no never treated units.
mask <- base::min(DF[[gname]], DF[[tname]])
DF[[gname]] <- DF[[gname]] - mask + 1
DF[[tname]] <- DF[[tname]] - mask + 1
DF <- did::pre_process_did(yname                  = yname,
                         tname                  = tname,
                         idname                 = idname,
                         gname                  = gname,
                         xformla                = xformla,
                         data                   = DF,
                         panel                  = panel,
                         allow_unbalanced_panel = allow_unbalanced_panel,
                         control_group          = control_group,
                         anticipation           = anticipation,
                         weightsname            = weightsname,
                         est_method             = est_method,
                         base_period            = "universal")$data

# First do some sanity checks
# NB: As of 2023-06, did::pre_process_did re-codes nevertreated as 0s
binsel <- if (test.option) TRUE else (DF[[tname]] < DF[[gname]]) | (DF[[gname]] == 0)
yvals  <- NULL
nvals  <- Inf
if(base::is.null(nbins) & base::is.null(binpoints)){
yvals <- base::sort(base::unique(DF[binsel, yname]))
nvals <- base::length(yvals)
if( nvals <= 1 ){
  base::stop("outcome must take at least two values")
}
nbins <- base::min(20, nvals)
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
diff1  <- binpoints[1] - yrange[1]
diffn  <- binpoints[base::length(binpoints)] - yrange[2]
if( (diff1 > 0) & (base::abs(diff1) > .Machine$double.eps^(1/2)) ){
  binpoints <- base::c(yrange[1], binpoints)
}
if( (diffn < 0) & (base::abs(diffn) > .Machine$double.eps^(1/2)) ){
  binpoints <- base::c(binpoints, yrange[2])
}
if( (diff1 < 0) & (diffn > 0) ){
  base::stop("Provided binpoints group all values into single bin.")
}
}

#  Build the bins
bin <- base::rep(NA, base::NROW(DF))
if ( nvals < 20 ) {
bins <- base::factor(DF[binsel, yname], levels=yvals)
base::warning("treating outcome as discrete; pass nbins or binpoints to modify this behavior")
} else {
bins <- base::cut(DF[binsel, yname],
                  breaks = if (base::is.null(nbins)) binpoints else nbins,
                  include.lowest = TRUE,
                  labels = NULL)
}
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
# Initialize matrix of influence functions
inf_function <- base::matrix(NA,
                           nrow = n_ids,
                           ncol = n_unique_bin)
point_estimates <- base::rep(NA, times = n_unique_bin)

#----------------------------------------------------------------------------
# Compute aggte from did package using each bin as outcome
run_bin <- function(j, test.option=FALSE){
    outname <- base::paste0("outcome_bin", as.character(j))

    # NB: As of 2023-06, did::pre_process_did re-codes nevertreated as 0s
    if ( test.option ) {
      DF[[outname]] <- (bin == unique_bin[j])
    } else {
      DF[[outname]] <- -(bin == unique_bin[j])
      DF[[outname]] <- base::ifelse((0 < DF[[gname]]) & (DF[[gname]] <= DF[[tname]]), 0, DF[[outname]])
    }
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

    names(out_bins$att) <- paste("g=", out_bins$group - 1 + mask, ", t=", out_bins$t - 1 + mask, sep="")
    return(list(aggt_param=aggt_param, att=out_bins$att))
}

#######################################################################
#                                                                     #
#                          Copied from didFF                          #
#                                                                     #
#######################################################################

results <- base::lapply(1:n_unique_bin, run_bin, test.option=FALSE)
# error
results <- base::lapply(1:n_unique_bin, run_bin, test.option=TRUE)

j <- 1
outname <- base::paste0("outcome_bin", as.character(j))
DF[[outname]] <- (bin == unique_bin[j])
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
