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

data_bins <- did::pre_process_did(yname                  = yname,
                                  tname                  = tname,
                                  idname                 = idname,
                                  gname                  = gname,
                                  xformla                = xformla,
                                  data                   = data_filtered,
                                  panel                  = panel,
                                  allow_unbalanced_panel = allow_unbalanced_panel,
                                  control_group          = control_group,
                                  anticipation           = anticipation,
                                  est_method             = est_method,
                                  base_period            = "universal")$data
bins = base::cut(data_bins[[yname]],
                 breaks = 20,
                 include.lowest = TRUE,
                 labels = NULL)
binpoints = base::unique(base::as.numeric(base::sub("[^,]*,([^]]*)\\]", "\\1", bins)))

#-----------------------------------------------------------------------------
# errors
test_that("didFF stops if yname not in data.", {
  expect_error(
    didFF(
      data   = data_filtered,
      yname  = paste0(yname, "not in data"),
      tname  = tname,
      idname = idname,
      gname  = gname
    ), "Must provide yname that is part of data."
  )
})

test_that("didFF stops if tname not in data.", {
  expect_error(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = paste0(tname, "not in data"),
      idname = idname,
      gname  = gname
    ), "Must provide tname that is part of data."
  )
})

test_that("didFF stops if idname not in data.", {
  expect_error(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = paste0(idname, "not in data"),
      gname  = gname
    ), "Must provide idname that is part of data."
  )
})

test_that("didFF stops if gname not in data.", {
  expect_error(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = paste0(gname, "not in data")
    ), "Must provide gname that is part of data."
  )
})

test_that("didFF stops if nbins and binpoints both specified.", {
  expect_error(
    didFF(
      data      = data_filtered,
      yname     = yname,
      tname     = tname,
      idname    = idname,
      gname     = gname,
      nbins     = nbins,
      binpoints = binpoints
    ), "Can only specify one of nbins or binpoints."
  )
})

test_that("didFF stops if nbins = 1", {
  expect_error(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      nbins  = 1
    ), "Must provide nbins at least equal to 2"
  )
})

test_that("didFF stops if binpoints length 0.", {
  expect_error(
    didFF(
      data      = data_filtered,
      yname     = yname,
      tname     = tname,
      idname    = idname,
      gname     = gname,
      binpoints = numeric()
    ), "Must provide at least 1 binpoints"
  )
})

test_that("didFF stops if binpoints has one group.", {
  expect_error(
    didFF(
      data      = data_filtered,
      yname     = yname,
      tname     = tname,
      idname    = idname,
      gname     = gname,
      binpoints = c(-Inf, Inf)
    ), "Provided binpoints group all values into single bin."
  )
})

test_that("didFF stops if aggte type not valid.", {
  expect_error(
    didFF(
      data       = data_filtered,
      yname      = yname,
      tname      = tname,
      idname     = idname,
      gname      = gname,
      aggte_type = paste0(aggte_type, "not allowed")
    ), "aggte_type must be equal to `simple`, `dynamic`, `group`, or `calendar`."
  )
})

test_that("didFF stops if invalid formula.", {
  expect_error(
    didFF(
      data    = data_filtered,
      yname   = yname,
      tname   = tname,
      idname  = idname,
      gname   = gname,
      xformla = data_filtered[["lpop"]]
    ), "invalid formula"
  )
  expect_error(
    didFF(
      data    = data_filtered,
      yname   = yname,
      tname   = tname,
      idname  = idname,
      gname   = gname,
      xformla = ~ `not in data`
    ), "object 'not in data' not found"
  )
})

test_that("didFF stops if invalid control_group", {
  expect_error(
    didFF(
      data    = data_filtered,
      yname   = yname,
      tname   = tname,
      idname  = idname,
      gname   = gname,
      control_group = "not allowed"
    ), "control_group must be either 'nevertreated' or 'notyettreated'"
  )
})

test_that("didFF stops if invalid no never-treated", {
  data_filtered[data_filtered[[gname]] == Inf, gname] <- -1
  expect_error(
    didFF(
      data    = data_filtered,
      yname   = yname,
      tname   = tname,
      idname  = idname,
      gname   = gname
    ), "No cohorts g=Inf or g=0 found; assuming no cohorts are never-treated. Use option nevertreated to specify the never-treated cohort."
  )

  data_filtered[data_filtered[[gname]] == -1, gname] <- 0
  data_filtered[[tname]] <- data_filtered[[tname]] - 2004
  expect_error(
    didFF(
      data    = data_filtered,
      yname   = yname,
      tname   = tname,
      idname  = idname,
      gname   = gname
    ), "No never-treated cohorts identified: You have observations with g=0 and at least one time period t <= 0. We assume this means that cohort's treatment started at time 0; if instead this means those units are never-treated, use option nevertreated=0 instead."
  )
})
