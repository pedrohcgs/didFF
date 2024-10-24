library(testthat)
library(didFF)
library(did)
set.seed(1729)

# Here we would put a suite of tests where we expect a specific result
# to test the function is working as expected (test_errors.R and
# test_base.R just test whether the options are correctly coded, not
# whether they are correctly implemented).

#-----------------------------------------------------------------------------
yname                  = "y"
tname                  = "t"
idname                 = "i"
gname                  = "g"
nbins                  = 20
allow_unbalanced_panel = FALSE

#-----------------------------------------------------------------------------

## Balanced panel
stagtest_genbalanced <- function(ni, nt, offset=123, frac.never=0.3) {
  t <- 1:nt + offset
  dat <- data.frame(t = rep(t, each = ni))
  dat$i <- ave(dat$t, dat$t, FUN = seq_along)
  dat$g <- offset + 1 + runif(ni * nt) * nt
  dat$g <- ave(dat$g, dat$i, FUN = function(x) ceiling(x[length(x)]))
  dat$y <- as.numeric(dat$t >= dat$g) + rnorm(ni * nt, sd = 3)
  dat$w <- abs(as.numeric(dat$t <= dat$g) + rnorm(ni * nt, sd = 3)) + 0.1
  dat[dat$i <= ceiling(frac.never * ni), "g"] <- Inf
  dat[order(dat$i, dat$t),]
}
## Unbalanced panel
stagtest_genasync <- function(ni, nt, offset=0, frac.never=0.2) {
  i <- 1:ni
  Nt <- nt - ceiling(runif(ni) * nt/2)
  dat <- data.frame(i = rep(i, times = Nt), Nt = rep(Nt, times = Nt))
  dat$t <- ave(dat$i, dat$i, FUN = function(x) 1:length(x) + offset)
  dat$g <- rep(NA_real_, sum(Nt))

  sel0 <- dat$i %% 4 == 0
  sel1 <- dat$i %% 4 == 1
  sel2 <- dat$i %% 4 == 2
  sel3 <- dat$i %% 4 == 3
  dat$g[sel0] <- offset + runif(sum(sel0)) * dat$Nt[sel0] / 2 + dat$Nt[sel0] / 2
  dat$g[sel1] <- offset + runif(sum(sel1)) * dat$Nt[sel1] * 2 + dat$Nt[sel1] / 2
  dat$g[sel2] <- offset + runif(sum(sel2)) * dat$Nt[sel2]     + dat$Nt[sel2] / 2
  dat$g[sel3] <- offset + runif(sum(sel3)) * dat$Nt[sel3]     + dat$Nt[sel3] / 2

  dat$g <- ave(dat$g, dat$i, FUN = function(x) ceiling(x[length(x)]))
  dat$y <- as.numeric(dat$t >= dat$g) + rnorm(sum(Nt), sd = 3)
  dat$w <- abs(as.numeric(dat$t <= dat$g) + rnorm(sum(Nt), sd = 3)) + 0.1

  dat[dat$i <= ceiling(frac.never * ni), "g"] <- Inf
  dat[dat$g >max(dat$t), "g"] <- Inf
  dat[order(dat$i, dat$t),]
}

# function to run for balanced panel
baserun_balanced <- function(dat) {
  # NB: It's OK not to mask in this example bc time starts after offset (default 123)
  dat[dat[[gname]]==-1, gname] <- 0
  data_bins <- base::suppressWarnings(
    did::pre_process_did(yname                  = yname,
                         tname                  = tname,
                         idname                 = idname,
                         gname                  = gname,
                         data                   = dat,
                         allow_unbalanced_panel = FALSE,
                         base_period            = "universal")$data
  )
  binsel    <- (data_bins[[tname]] < data_bins[[gname]]) | (data_bins[[gname]] == 0)
  yrange    <- range(data_bins[binsel, yname])
  bins      <- base::cut(data_bins[binsel, yname],
                         breaks = nbins,
                         include.lowest = TRUE,
                         labels = NULL)
  #binpoints <- base::sort(base::unique(base::as.numeric(base::sub("[^,]*,([^]]*)\\]", "\\1", bins))))
  binpoints <- seq(yrange[1], yrange[2], diff(yrange) / nbins)

  expect_silent(
    results <- didFF(
      data          = dat,
      yname         = yname,
      tname         = tname,
      idname        = idname,
      gname         = gname
    )
  )

  expect_silent(didFF(
    data   = dat,
    yname  = yname,
    tname  = tname,
    idname = idname,
    gname  = gname,
    weightsname = "w"
  ))

  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      est_method = "dr"
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      xformla = ~ 1
    )
  )

  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      panel  = FALSE
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      allow_unbalanced_panel = TRUE
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      control_group = "notyettreated"
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      nbins  = 50
    )
  )

  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      binpoints = binpoints
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      numSims = 100
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      seed   = 1729
    )
  )

  # types <- c("simple", "dynamic", "group", "calendar")
  types <- c("simple", "dynamic", "group")
  for(type in types) {

    expect_silent(
      result <- didFF(
        data   = dat,
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
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      pl     = TRUE,
      cores  = 2
    )
  )
}

test_that("multi-period staggered balanced panel data frame runs", {
  baserun_balanced(stagtest_genbalanced(200, 5))
})


# Run for unbalanced panel
baserun_unbalanced <- function(dat) {
  # NB: It's OK not to mask in this example bc time starts after offset (default 123)
  #dat[dat[[gname]]==-1, gname] <- 0
  data_bins <- base::suppressWarnings(
    did::pre_process_did(yname                  = yname,
                         tname                  = tname,
                         idname                 = idname,
                         gname                  = gname,
                         data                   = dat,
                         allow_unbalanced_panel = TRUE,
                         base_period            = "universal")$data
  )
  binsel    <- (data_bins[[tname]] < data_bins[[gname]]) | (data_bins[[gname]] == 0)
  yrange    <- range(data_bins[binsel, yname])
  bins      <- base::cut(data_bins[binsel, yname],
                         breaks = nbins,
                         include.lowest = TRUE,
                         labels = NULL)
  #binpoints <- base::sort(base::unique(base::as.numeric(base::sub("[^,]*,([^]]*)\\]", "\\1", bins))))
  binpoints <- seq(yrange[1], yrange[2], diff(yrange) / nbins)

  expect_silent(
    results <- didFF(
      data          = dat,
      yname         = yname,
      tname         = tname,
      idname        = idname,
      gname         = gname,
      panel         = TRUE,
      allow_unbalanced_panel = TRUE
    )
  )

  expect_silent(didFF(
    data   = dat,
    yname  = yname,
    tname  = tname,
    idname = idname,
    gname  = gname,
    weightsname = "w",
    allow_unbalanced_panel = TRUE
  ))

  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      est_method = "dr",
      allow_unbalanced_panel = TRUE
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      xformla = ~ 1,
      allow_unbalanced_panel = TRUE
    )
  )

  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      panel  = FALSE
    )
  )


  expect_warning(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      allow_unbalanced_panel = FALSE
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      control_group = "notyettreated",
      allow_unbalanced_panel = TRUE
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      nbins  = 50,
      allow_unbalanced_panel = TRUE
    )
  )

  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      binpoints = binpoints,
      allow_unbalanced_panel = TRUE
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      numSims = 100,
      allow_unbalanced_panel = TRUE
    )
  )


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      seed   = 1729,
      allow_unbalanced_panel = TRUE
    )
  )

  # types <- c("simple", "dynamic", "group", "calendar")
  types <- c("simple", "dynamic", "group")
  for(type in types) {

    expect_silent(
     didFF(
        data   = dat,
        yname  = yname,
        tname  = tname,
        idname = idname,
        gname  = gname,
        aggte_type = type,
        allow_unbalanced_panel = TRUE
      )
    )
  }


  expect_silent(
    didFF(
      data   = dat,
      yname  = yname,
      tname  = tname,
      idname = idname,
      gname  = gname,
      pl     = TRUE,
      cores  = 2,
      allow_unbalanced_panel = TRUE
    )
  )
}
test_that("multi-period staggered unbalanced panel data frame runs", {
  baserun_unbalanced(stagtest_genasync(400, 5))
})
