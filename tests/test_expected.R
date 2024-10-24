library(testthat)
library(didFF)
library(did)
set.seed(1729)

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
#xformla                = ~ lpop + I(lpop^2)
est_method             = "dr"
control_group          = "nevertreated"
nbins                  = 20
anticipation           = 0
allow_unbalanced_panel = FALSE
panel                  = TRUE
aggte_type             = "group"

# NB: It's OK not to mask in this example bc time starts in the 2000s
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
binsel    = (data_bins[[tname]] < data_bins[[gname]]) | (data_bins[[gname]] == 0)
yrange    = range(data_bins[binsel, yname])
bins      = base::cut(data_bins[binsel, yname],
                      breaks = nbins,
                      include.lowest = TRUE,
                      labels = NULL)
binpoints = base::sort(base::unique(base::as.numeric(base::sub("[^,]*,([^]]*)\\]", "\\1", bins))))
binpoints = seq(yrange[1], yrange[2], diff(yrange) / nbins)

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
  expect_equal(base::sum(results$table[,2]), 1, tol=.Machine$double.eps^(1/2))

  # do run by hand
  bins <- base::cut(data_filtered[[yname]],
                    breaks = if (base::is.null(nbins)) binpoints else nbins,
                    include.lowest = TRUE,
                    labels = NULL,
                    dig.lab = 21)
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
      data_filtered$lemp_1 <- -((data_filtered$lemp <= thresh[s]) - (data_filtered$lemp <= thresh[s-1]))
    }
    #base::sum((data_filtered$lemp_1==-1) - (bin==s))
    data_filtered$lemp_1 <- base::ifelse(
      data_filtered$first.treat <= data_filtered$year,
      0,
      data_filtered$lemp_1
    )
    test_run_manual <- suppressMessages(
      did::att_gt(
      data          = data_filtered,
      yname         = "lemp_1",
      tname         = tname,
      idname        = idname,
      gname         = gname,
      est_method    = est_method,
      xformla       = NULL,
      control_group = control_group,
      cband = FALSE,
      base_period = "universal",
      bstrap = FALSE
      )
    )
    manual_test[s,2] <- test_run_manual$att[2]
    manual_test[s,3] <- test_run_manual$se[2]
  }
  # ttests = base::min(manual_test[,2]/manual_test[,3], na.rm = TRUE)

  expect_equal(results$table[,2], manual_test[,2], tol=.Machine$double.eps^(1/2))
})

#-----------------------------------------------------------------------------
# The density shouldn't depend on the outcomes for already treated units

test_that("didFF does not depend on outcome values in treated periods", {
  df1 <- data_filtered
  df2 <- subset(data_filtered, (first.treat != year) | (lpop > 5))
  df3 <- data_filtered
  df3[df3$first.treat == df3$year, yname] <- rnorm(sum(df3$first.treat == df3$year))

  for ( meth in c("dr", "reg", "ipw") ) {
    resultsA <- didFF(
      data       = df1,
      yname      = yname,
      tname      = tname,
      idname     = idname,
      gname      = gname,
      est_method = meth,
      binpoints  = binpoints
    )
    resultsB <- didFF(
      data       = df2,
      yname      = yname,
      tname      = tname,
      idname     = idname,
      gname      = gname,
      est_method = meth,
      nbins      = nbins,
      allow_unbalanced_panel=TRUE
    )
    resultsC <- didFF(
      data       = df3,
      yname      = yname,
      tname      = tname,
      idname     = idname,
      gname      = gname,
      est_method = meth,
      nbins      = nbins
    )

    denA <- resultsA$table$implied_density
    denB <- resultsB$table$implied_density
    denC <- resultsC$table$implied_density
    expect_equal(denA, denB, tol=.Machine$double.eps^(1/2))
    expect_equal(denA, denC, tol=.Machine$double.eps^(1/2))
    expect_equal(denB, denC, tol=.Machine$double.eps^(1/2))
  }

  sub_filtered <- data_filtered$first.treat == data_filtered$year
  data_filtered[[yname]] <- rnorm(NROW(data_filtered)) * 3
  data_filtered[sub_filtered, yname] <- rnorm(sum(sub_filtered)) * 10

  sub_bins <- base::cut(data_filtered[[yname]],
                        breaks = 20,
                        include.lowest = TRUE,
                        labels = NULL,
                        dig.lab = 21)
  sub_binpoints <- base::unique(base::as.numeric(base::sub("[^,]*,([^]]*)\\]", "\\1", sub_bins)))

  df1 <- data_filtered
  df2 <- subset(data_filtered, (first.treat != year) | (lpop > 5))
  df3 <- data_filtered
  df3[df3$first.treat == df3$year, yname] <- 0

  for ( meth in c("dr", "reg", "ipw") ) {
    resultsA <- base::suppressWarnings(
      didFF(
      data       = df1,
      yname      = yname,
      tname      = tname,
      idname     = idname,
      gname      = gname,
      est_method = meth,
      control_group = c("nevertreated", "notyettreated"),
      binpoints  = binpoints
      )
    )
    resultsB <- base::suppressWarnings(
      didFF(
      data       = df2,
      yname      = yname,
      tname      = tname,
      idname     = idname,
      gname      = gname,
      est_method = meth,
      binpoints  = binpoints,
      control_group = "notyettreated",
      allow_unbalanced_panel=TRUE
      )
    )
    resultsC <- base::suppressWarnings(
      didFF(
      data       = df3,
      yname      = yname,
      tname      = tname,
      idname     = idname,
      gname      = gname,
      est_method = meth,
      binpoints  = binpoints,
      control_group = "nevertreated"
      )
    )

    denA <- resultsA$table$implied_density
    denB <- resultsB$table$implied_density
    denC <- resultsC$table$implied_density
    expect_equal(denA, denB, tol=.Machine$double.eps^(1/2))
    expect_equal(denA, denC, tol=.Machine$double.eps^(1/2))
    expect_equal(denB, denC, tol=.Machine$double.eps^(1/2))
  }
})

#-----------------------------------------------------------------------------
# Check explicit expression

# this is all hard-coded
test_that("didFF recovers exact theoretical densities in discrete case", {
  for (i in 1:10) {
    th  <- round(runif(1) * 0.5 + 0.4, 2)
    Dt  <- 1:10
    Dd  <- 1:10
    # gt0 <- runif(length(Dt)) * 0.5 + 0.5
    # gt1 <- runif(length(Dt)) * 0.5 + 0.5
    gt0 <- runif(length(Dt)) * 0.3 + 0.3
    gt1 <- runif(length(Dt)) * 0.7 + 0.7
    gt0 <- round(gt0/sum(gt0), 2)
    gt1 <- round(gt1/sum(gt1), 2)
    # hd0 <- runif(length(Dd))
    # hd1 <- runif(length(Dd))
    hd0 <- runif(length(Dt)) * 1.7 + 0.7
    hd1 <- runif(length(Dt)) * 1.3 + 0.3
    hd0 <- round(hd0/sum(hd0), 2)
    hd1 <- round(hd1/sum(hd1), 2)
    p00 <- round(th * gt0 + (1-th) * hd0, 2)
    p01 <- round(th * gt0 + (1-th) * hd1, 2)
    p10 <- round(th * gt1 + (1-th) * hd0, 2)
    p11 <- round(th * gt1 + (1-th) * hd1, 2)
    n   <- 10
    nth <- 10
    tol <- 1e-1
    eps <- max(abs(th*nth-round(nth * th)))
    while ( abs(eps) > tol ) {
      nth <- nth + 1
      eps <- max(abs(th*nth-round(nth * th)))
    }
    pr  <- c(gt0, gt1, hd0, hd1)
    eps <- max(abs(pr*n-round(n * pr)))
    while ( abs(eps) > tol ) {
      n   <- n + 1
      eps <- max(abs(pr*n-round(n * pr)))
    }

    nt0 <- nth * th
    nt1 <- nth - nt0
    gr0 <- round(nt0 * n * gt0)
    gr1 <- round(nt0 * n * gt1)
    hr0 <- round(nt1 * n * hd0)
    hr1 <- round(nt1 * n * hd1)
    while ( sum(gr0) < round(nt0 * n) ) gr0[1] <- gr0[1] + 1
    while ( sum(gr0) > round(nt0 * n) ) gr0[1] <- gr0[1] - 1
    while ( sum(gr1) < round(nt0 * n) ) gr1[1] <- gr1[1] + 1
    while ( sum(gr1) > round(nt0 * n) ) gr1[1] <- gr1[1] - 1
    while ( sum(hr0) < round(nt1 * n) ) hr0[1] <- hr0[1] + 1
    while ( sum(hr0) > round(nt1 * n) ) hr0[1] <- hr0[1] - 1
    while ( sum(hr1) < round(nt1 * n) ) hr1[1] <- hr1[1] + 1
    while ( sum(hr1) > round(nt1 * n) ) hr1[1] <- hr1[1] - 1
    gt0 <- gr0/sum(gr0)
    gt1 <- gr1/sum(gr1)
    hd0 <- hr0/sum(hr0)
    hd1 <- hr1/sum(hr1)
    p00 <- th * gt0 + (1-th) * hd0
    p01 <- th * gt0 + (1-th) * hd1
    p10 <- th * gt1 + (1-th) * hd0
    p11 <- th * gt1 + (1-th) * hd1
    expect_equal(sum(p00), 1, tol=.Machine$double.eps^(1/2))
    expect_equal(sum(p01), 1, tol=.Machine$double.eps^(1/2))
    expect_equal(sum(p10), 1, tol=.Machine$double.eps^(1/2))
    expect_equal(sum(p11), 1, tol=.Machine$double.eps^(1/2))

    t  <- 0:1
    n  <- (nt0 + nt1) * n
    ti <- kronecker(t, rep(1, 2 * n))
    ni <- 2 * n * length(t)
    di <- kronecker(rep(t, 2), rep(1, n))
    id <- rep(1:(2*n), length(t))
    Fy <- numeric(ni)
    Fy[ti == 0 & di == 0] <- c(rep(Dt, gr0), rep(Dd, hr0))
    Fy[ti == 0 & di == 1] <- c(rep(Dt, gr0), rep(Dd, hr1))
    Fy[ti == 1 & di == 0] <- c(rep(Dt, gr1), rep(Dd, hr0))
    if ( runif(1) > 0.5 ) {
        Fy[ti == 1 & di == 1] <- c(rep(Dt, gr1), rep(Dd, hr1)) + 5
        pad <- TRUE
    } else {
        Fy[ti == 1 & di == 1] <- sample(1:10, size=sum(ti == 1 & di == 1), replace=T, prob=1:10)
        pad <- FALSE
    }

    di[di == 0] <- Inf
    DF  <- data.frame(y=Fy, t=ti, i=id, g=di)
    res <- didFF(data = DF, yname = "y", tname = "t", idname = "i", gname = "g", binpoints = 0:10)
   expect_equal(res$table$implied_density, p11, tol=.Machine$double.eps^(1/2))

    sel0 <- ti == 0 & di == Inf
    sel1 <- ti == 0 & di == 1
    sel2 <- ti == 1 & di == Inf
    df0  <- data.frame(prop.table(table(Fy[sel0])))
    df1  <- data.frame(prop.table(table(Fy[sel1])))
    df2  <- data.frame(prop.table(table(Fy[sel2])))
    colnames(df0) <- c("y", "F0")
    colnames(df1) <- c("y", "F1")
    colnames(df2) <- c("y", "F2")
    dfM <- merge(df0, df1, by="y", all=TRUE)
    dfM <- merge(dfM, df2, by="y", all=TRUE)
    dfM[is.na(dfM[["F0"]]), "F0"] <- 0
    dfM[is.na(dfM[["F1"]]), "F1"] <- 0
    dfM[is.na(dfM[["F2"]]), "F2"] <- 0
    dfM <- dfM[order(dfM$y),]
   expect_equal(res$table$implied_density, dfM$F1 + dfM$F2 - dfM$F0, tol=.Machine$double.eps^(1/2))

    F_t1_d1_implied <- res$table$implied_density
    F_t1_d1_actual  <- prop.table(table(Fy[ti == 1 & di == 1]))
    if ( runif(1) > 0.5 ) {
        res <- didFF(data             = DF,
                     yname            = "y",
                     tname            = "t",
                     idname           = "i",
                     gname            = "g",
                     distDD           = TRUE,
                     binpoints        = 0:15)
    } else {
        res <- distDD(data             = DF,
                      yname            = "y",
                      tname            = "t",
                      idname           = "i",
                      gname            = "g",
                      binpoints        = 0:15)
    }
    if ( pad ) {
     expect_equal(as.vector(c(rep(0, 5), F_t1_d1_actual) - c(F_t1_d1_implied, rep(0, 5))),
                  res$table$test.estimates,
                  tol=.Machine$double.eps^(1/2))
    } else {
     expect_equal(as.vector(F_t1_d1_actual - F_t1_d1_implied),
                  res$table$test.estimates,
                  tol=.Machine$double.eps^(1/2))
    }
  }
})

#-----------------------------------------------------------------------------
# multiple periods, cohorts

test_that("didFF recovers exact theoretical densities with multiple times/cohorts", {
  for (i in 1:10) {
    # Sorry for the notation: k is time and j is cohort
    kk  <- 4
    th  <- round(runif(1) * 0.6 + 0.2, 2)
    Dt  <- 1:10
    Dd  <- 1:10

    # nt <- double(kk)
    gt <- hd <- gr <- hr <- matrix(NA, kk, length(Dt))
    p  <- list()
    for (k in 1:kk) {
      gt[k,] <- runif(length(Dt)) * 0.5 + 0.5
      gt[k,] <- round(gt[k,]/sum(gt[k,]), 2)

      hd[k,] <- runif(length(Dd))
      hd[k,] <- round(hd[k,]/sum(hd[k,]), 2)

      p[[k]] <- matrix(NA, kk, length(Dt))
    }

    for (k in 1:kk) {
      for (j in 1:kk) {
        p[[k]][j,] <- round(th * gt[k,] + (1-th) * hd[j,], 2)
      }
    }

    n   <- 10
    nth <- 10
    tol <- 1e-1
    eps <- max(abs(th*nth-round(nth * th)))
    while ( abs(eps) > tol ) {
      nth <- nth + 1
      eps <- max(abs(th*nth-round(nth * th)))
    }
    pr  <- c(gt, hd)
    eps <- max(abs(pr*n-round(n * pr)))
    while ( abs(eps) > tol ) {
      n   <- n + 1
      eps <- max(abs(pr*n-round(n * pr)))
    }

    nt0 <- nth * th
    nt1 <- nth - nt0
    for (k in 1:kk) {
      gr[k,] <- round(nt0 * n * gt[k,])
      hr[k,] <- round(nt1 * n * hd[k,])
    }

    for (k in 1:kk) {
      while ( sum(gr[k,]) < round(nt0 * n) ) gr[k,1] <- gr[k,1] + 1
      while ( sum(gr[k,]) > round(nt0 * n) ) gr[k,1] <- gr[k,1] - 1
      while ( sum(hr[k,]) < round(nt1 * n) ) hr[k,1] <- hr[k,1] + 1
      while ( sum(hr[k,]) > round(nt1 * n) ) hr[k,1] <- hr[k,1] - 1
    }

    for (k in 1:kk) {
      gt[k,] <- gr[k,]/sum(gr[k,])
      hd[k,] <- hr[k,]/sum(hr[k,])
    }

    for (k in 1:kk) {
      for (j in 1:kk) {
        p[[k]][j,] <- th * gt[k,] + (1-th) * hd[j,]
      }
    }

    for (k in 1:kk) {
      for (j in 1:kk) {
        expect_equal(sum(p[[k]][j,]), 1, tol=.Machine$double.eps^(1/2))
      }
    }

    t  <- 0:(kk-1)
    n  <- (nt0 + nt1) * n
    ti <- kronecker(t, rep(1, kk * n))
    ni <- kk * n * length(t)
    di <- kronecker(rep(t, kk), rep(1, n))
    id <- rep(1:(kk * n), length(t))
    Fy <- numeric(ni)
    for (k in 1:kk) {
      for (j in 1:kk) {
        Fy[ti == (k-1) & di == (j-1)] <- c(rep(Dt, gr[k,]), rep(Dd, hr[j,]))
      }
    }

    di[di == 0] <- Inf
    DF  <- data.frame(y=Fy, t=ti, i=id, g=di)
    res <- base::suppressWarnings(
      didFF(data = DF,
            yname = "y",
            tname = "t",
            idname = "i",
            gname = "g")
    )

    # ATT = 0 if not yet treated (cohort < t)
    for (j in 2:kk) {
      for (k in j:kk) {
        # print(c(j-1, k-1))
        expect_equal(unname(p[[k]][j,]), unname(res$att[(j-2) * kk + k,]), tol=.Machine$double.eps^(1/2))
      }
    }

    for (gg in 1:(kk-1)) {
      for (tt in gg:(kk-1)) {
        sel0 <- (ti == (gg-1)) & di == Inf
        sel1 <- (ti == (gg-1)) & di == gg
        sel2 <- (ti == tt)     & di == Inf
        df0  <- data.frame(prop.table(table(Fy[sel0])))
        df1  <- data.frame(prop.table(table(Fy[sel1])))
        df2  <- data.frame(prop.table(table(Fy[sel2])))
        colnames(df0) <- c("y", "F0")
        colnames(df1) <- c("y", "F1")
        colnames(df2) <- c("y", "F2")
        dfM <- merge(df0, df1, by="y", all=TRUE)
        dfM <- merge(dfM, df2, by="y", all=TRUE)
        dfM[is.na(dfM[["F0"]]), "F0"] <- 0
        dfM[is.na(dfM[["F1"]]), "F1"] <- 0
        dfM[is.na(dfM[["F2"]]), "F2"] <- 0
        dfM <- dfM[order(dfM$y),]

        expect_equal(unname(res$att[(gg - 1)*kk+tt+1,]),
                     dfM$F1 + dfM$F2 - dfM$F0,
                     tol=.Machine$double.eps^(1/2))
      }
    }
  }
})

#-----------------------------------------------------------------------------
# p-value simulation

# Please run locally
# test_that("didFF rejects roughly 5% of the time", {
#   B  <- 1000
#   P  <- numeric(B)
#   for (b in 1:B) {
#     t  <- 0:1
#     n  <- 2000
#     ub <- 10
#     ti <- kronecker(t, rep(1, 2 * n))
#     ni <- 2 * n * length(t)
#     di <- kronecker(rep(t, 2), rep(1, n))
#     id <- rep(1:(2*n), length(t))
#     Fy <- numeric(ni)
#     Fy[ti == 0 & di == 0] <- sample(1:ub, n, T)
#     Fy[ti == 0 & di == 1] <- sample(1:ub, n, T)
#     Fy[ti == 1 & di == 0] <- sample((ub-1):ub, n, T)
#     Fy[ti == 1 & di == 1] <- 0
#
#     di[di == 0] <- Inf
#     DF   <- data.frame(y=Fy, t=ti, i=id, g=di)
#     res  <- didFF(data = DF, yname = "y", tname = "t", idname = "i", gname = "g", binpoints = 0:ub)
#     P[b] <- res$pval
#     # print(res$pval)
#     # print(res$table)
#     # round(res$table$implied_density, 3)
#   }
#   expect_equal(as.numeric(table(cut(P, breaks=c(0, 0.05, 0.1)))/length(P)), c(0.05, 0.05), 0.001)
# })
