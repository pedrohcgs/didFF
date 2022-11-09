#devtools::install_github("pedrohcgs/DRDID", dependencies = TRUE)
#devtools::install_github("bcallaway11/did", dependencies = TRUE)

library(did)
#library(didFF) #load the TestFunctional Form package
library(dplyr)
library(here)

DF = did::mpdta %>%
  filter((year==2003) | (year == 2004))

# Set number of bins
idvar = "countyreal"
yvar = "lemp"
tvar = "year"
gname = "first.treat"
nbins = 100

# Store not-yet-treated as Infinity
DF$first.treat <- ifelse((DF$first.treat>2004) |
                           DF$first.treat==0,
                         Inf,
                         DF$first.treat)

# assume outcome is discrete
DF$outcome_discrete <- (DF[[yvar]] > quantile(DF[[yvar]], probs = 0.25)) +
  (DF[[yvar]] > quantile(DF[[yvar]], probs = 0.50)) +
  (DF[[yvar]] > quantile(DF[[yvar]], probs = 0.75))

yvar = "outcome_discrete"
# Get the bins (regardless of treatment group)
bin <- as.numeric(cut(DF[[yvar]],
                      breaks=nbins,
                      include.lowest=TRUE,
                      labels=FALSE))

level <- as.numeric( sub("[^,]*,([^]]*)\\]",
                         "\\1",cut(DF[[yvar]],
                                   breaks=nbins,
                                   include.lowest=TRUE)) )
nha = cbind(bin,level)
y <- as.numeric(DF[[yvar]])
year <- as.numeric(DF[[tvar]])
G<-as.numeric(DF[[gname]])
id<-DF[[idvar]]

df1<-data.frame(id,bin,level,y,year,G)

# Get loop for each value of the bin variable
unique_bin <- sort(unique(df1$bin))
unique_level <- sort(unique(df1$level))
n_unique_bin <- length(unique_bin)
n_ids <- length(unique(df1$id))

uncentered_inf_function <- matrix(NA,
                                  nrow = n_ids,
                                  ncol = n_unique_bin)

time_s <- Sys.time()
for(j in 1:n_unique_bin){
  df1$outcome <- -(df1$bin==unique_bin[j])
  df1$outcome <- ifelse(df1$G<=df1$year, 0,
                        as.numeric(df1$outcome)
  )
  out <- suppressMessages(
    att_gt(yname = "outcome",
           gname = "G",
           idname = "id",
           tname = "year",
           control_group = "nevertreated",
           xformla = ~1,
           data = df1,
           est_method = "reg",
           bstrap = FALSE,
           cband = FALSE,
           base_period = "universal"
    )
  )
  aggt_param <- aggte(out, type = "simple", cband = FALSE,
                      bstrap = FALSE)

  uncentered_inf_function[,j] <- aggt_param$overall.att + aggt_param$inf.function$simple.att

}

# Compute the implied pdf for each bin

# Drop collinear covariates
qr.uncentered_inf_function <-  base::qr(uncentered_inf_function,
                           tol=1e-6,
                           LAPACK = FALSE)
rnk_cuncentered_inf_function <- qr.uncentered_inf_function$rank
keep_uncentered_inf_function <- qr.uncentered_inf_function$pivot[seq_len(rnk_cuncentered_inf_function)]
uncentered_inf_function <- uncentered_inf_function[,keep_uncentered_inf_function]


implied_density <- colMeans(uncentered_inf_function)
time_e <- Sys.time()
time_e - time_s
#Asymptotic Variance-covariance matrix
AsyVar <- crossprod(uncentered_inf_function -
                      matrix(rep(implied_density,n_ids),
                             ncol = dim(uncentered_inf_function)[2],
                             nrow = n_ids,
                             byrow = TRUE))/n_ids

# Compute std error for each
implied_density_varcov <- (AsyVar/n_ids)
#--------
# Run our current package to double check



setwd(here("R/"))
files.sources = list.files()
files.sources <- files.sources[-2]
sapply(files.sources, source)

start_t = 2003
end_t = 2004
DF$treated = as.numeric(DF$first.treat==2004)
treatmentvar = "treated" #treatment indicator
weight  =NULL
time_s_package <- Sys.time()
df1_package<-iDiscretize(DF,
                         idvar,
                         yvar,
                         tvar,
                         treatmentvar,
                         weight,
                         nbins)

implied_density_table<- (iDensity(df1_package,
                                start_t,
                                end_t))

implied_density_table <- implied_density_table[order(implied_density_table$level),]
implied_density_table2 <- tibble(level = unique_level[keep_uncentered_inf_function],
                                    implied_density_post = implied_density)

implied_density_table2$implied_density_post - implied_density_table$implied_density_post[keep_uncentered_inf_function]
time_e_package <- Sys.time()
time_e_package - time_s_package

implied_density_table$implied_density_post
implied_density

if (is.null(start_t)) {
  sy<-min(df2$year)
} else {
  sy<-start_t
}
if (is.null(end_t)) {
  ey<-max(df2$year)
} else {
  ey<-end_t
}
compare_pre_post <-
  df1_package %>%
  dplyr::filter((sy <= year) & (year <= ey)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(treated_in_period = max(D > 0)) %>% #was your state ever treated in this period
  dplyr::group_by(treated_in_period) %>%
  dplyr::filter((year == sy) | (year == ey)) %>% #filter to pre and post
  dplyr::arrange(bin, id, year)

  #Compute Density by year-treatment-bin
long_summary_table <-
  compare_pre_post %>%
  dplyr::group_by(year, level, treated_in_period) %>%
  dplyr::summarise(count = sum(w), .groups = "keep") %>%
  dplyr::group_by(year,treated_in_period) %>%
  dplyr::mutate(idensity = proportions(count)) %>%
  dplyr::select(-count)




new_test <- didFF_new(
  data = DF,
  yname = "outcome_discrete",
  tname = "year",
  idname = "countyreal",
  gname = "first.treat",
  weightsname = NULL, #"lpop",
  clustervars = NULL,
  est_method = "reg",
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
  aggte_type = "simple",
  pl = FALSE,
  cores=1
)


new_test3 <- didFF_new(
  data = DF,
  yname = "outcome_discrete",
  tname = "year",
  idname = "countyreal",
  gname = "first.treat",
  weightsname = NULL, #"lpop",
  clustervars = NULL,
  est_method = "reg",
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
  aggte_type = "calendar",
  pl = FALSE,
  cores=1
)

# nboots = 1000
# #Compute the implied_density for each bootstrap draw
# bootStrapResults <- purrr::map_dfr(.x=1:nboots,
#                                    .f = ~iDensity(boot_id(df1_package,"id",.x),
#                                                   start_t,
#                                                   end_t
#                                    )%>%
#                                      dplyr::mutate(nboot = .x))
# # #Compute the covariance matrix of moments over the bootstrap draws
# sigma <-
#   bootStrapResults %>%
#   dplyr::select(level, implied_density_post, nboot) %>%
#   tidyr::pivot_wider(names_from = level,
#                      id_cols = nboot,
#                      values_from = implied_density_post) %>%
#   dplyr::select(-nboot)%>%
#   as.matrix() %>%
#   cov()
#
