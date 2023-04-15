library(testthat)
library(didFF)
library(did)

# Here we would put a suite of tests where we expect a specific result
# to test the function is working as expected (test_errors.R and
# test_base.R just test whether the options are correctly coded, not
# whether they are correctly implemented).

#-----------------------------------------------------------------------------
# Base example
start_t = 2003 #pre-treatment year
end_t   = 2004 #post-treatment year
cohort  = 2004

data_filtered = subset(didFF::mw_df,  year == start_t | year == end_t)
data_filtered = subset(data_filtered, first.treat == cohort | first.treat == 0)

# Recode first.treat ==0 to first.treat==Inf
data_filtered$first.treat[data_filtered$first.treat == 0] <- Inf

# Parameters for the function
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
bins                   = base::cut(data_filtered[[yname]],
                                   breaks = 20,
                                   include.lowest = TRUE,
                                   labels = NULL)
binpoints              = base::unique(base::as.numeric(base::sub("[^,]*,([^]]*)\\]", "\\1", bins)))

#-----------------------------------------------------------------------------
# Example

test_that("didFF returns same as manual run", {
  results <- didFF(
    data          = data_filtered,
    yname         = yname,
    tname         = tname,
    idname        = idname,
    gname         = gname,
    est_method    = est_method,
    xformla       = xformla,
    control_group = control_group,
    nbins         = nbins
  )

  expect_lte(base::NROW(results$table), nbins)
  expect_gte(base::NROW(results$table), 2)
  expect_lte(results$pval, 1)
  expect_gte(results$pval, 0)
  expect_equal(base::sum(results$table[,2]), 1, tol=.Machine$double.eps^0.5)

  # do run by hand
  bins <- base::cut(data_filtered[[yname]],
                    breaks = if (base::is.null(nbins)) binpoints else nbins,
                    include.lowest = TRUE,
                    labels = NULL)
  bin  <- base::as.numeric(bins)
  bin2 <- base::levels(base::droplevels(bins))

  level_bin       <- base::as.numeric(base::sub("[^,]*,([^]]*)\\]", "\\1", bins))
  thresh          <- base::sort(base::unique(level_bin))
  manual_test     <- matrix(NA, nrow = base::length(thresh), ncol = 3)
  manual_test[,1] <- 1:base::length(thresh)

  for(s in 1:base::length(thresh)){
    if(s==1) {
      data_filtered$lemp_1 <- -((data_filtered$lemp <= thresh[s]))
    }  else {
      data_filtered$lemp_1 <- -((data_filtered$lemp <= thresh[s]) - (data_filtered$lemp < thresh[s-1]))
    }
    base::sum((data_filtered$lemp_1==-1) - (bin==s))
    data_filtered$lemp_1 <- base::ifelse(
      data_filtered$first.treat <= data_filtered$year,
      0,
      data_filtered$lemp_1
    )
    test_run_manual <- did::att_gt(
      data          = data_filtered,
      yname         = "lemp_1",
      tname         = tname,
      idname        = idname,
      gname         = gname,
      est_method    = est_method,
      xformla       = NULL,
      control_group = control_group
    )
    manual_test[s,2] <- test_run_manual$att
    manual_test[s,3] <- test_run_manual$se
  }
  ttests = base::min(manual_test[,2]/manual_test[,3], na.rm = TRUE)
})
