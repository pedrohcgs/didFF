# remove.packages("didFF")
# install.packages(".", repos=NULL, type="source")
# testthat::test_dir("tests")
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
data_filtered$weights <- sample(1:4, NROW(data_filtered), replace=TRUE)

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
binpoints =  sort(binpoints)
binpoints = c(range(data_bins[[yname]])[1], binpoints)
# Run the test
test_that("didFF base run with no errors", {
  result <- didFF(
    data          = data_filtered,
    yname         = yname,
    tname         = tname,
    idname        = idname,
    gname         = gname
  )
  expect_silent(result)
})

test_that("didFF base options run with no errors", {

  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      weightsname = "weights"
    )
  )


  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      est_method = "dr"
    )
  )


  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      xformla = ~ lpop
    )
  )


  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      panel  = FALSE
    )
  )

  expect_silent(
    didFF(
    data   = data_filtered,
    yname  = yname,
    tname  = tname,
    idname = idname,
    gname  = gname,
    allow_unbalanced_panel = TRUE
    )
  )

  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      control_group = "notyettreated"
    )
  )


  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      nbins  = 50
    )
  )

  expect_silent(
    didFF(
    data   = data_filtered,
    yname  = yname,
    tname  = tname,
    idname = idname,
    gname  = gname,
    binpoints = binpoints
    )
  )



  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      numSims = 100
    )
  )

  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      seed   = 1729
    )
  )

  types <- c("simple", "dynamic", "group", "calendar")
  for(type in types) {

    expect_silent(
      didFF(
        data   = data_filtered,
        yname  = yname,
        tname  = tname,
        idname = idname,
        gname  = gname,
        aggte_type = type
      )
    )
  }

  expect_silent(
    didFF(
      data   = data_filtered,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      pl     = TRUE,
      cores  = 2
    )
  )
})
