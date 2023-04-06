library(here)
library(tidyverse)
library(dplyr)
library(did)
#-----------------------------------------------------------------------------
# Load data
load(here("data/mw_df.rda"))
# Load the didFF function
source((here("R",'didFF.R')))
#-----------------------------------------------------------------------------
# Subset the data for simplicity
start_t = 2003 #pre-treatment year
end_t = 2004  #post-treatment year
cohort = 2004

data_filtered = mw_df %>%
  filter(year == start_t | year == end_t)%>%
  filter(first.treat == cohort | first.treat == 0)

# Recode first.treat ==0 to first.treat==Inf

data_filtered$first.treat[data_filtered$first.treat == 0] <- Inf

#-----------------------------------------------------------------------------
# Parameters for the function
yname = "lemp"
tname = "year"
idname = "countyreal"
gname="first.treat"
xformla=~1
xformla <- ~ lpop + I(lpop^2)
est_method = "dr"
control_group = "nevertreated"
nbins = 20
anticipation = 0
allow_unbalanced_panel = FALSE
panel = TRUE
aggte_type = "group"

# Run the test
test_run <- didFF(
  data = data_filtered,
  yname = yname,
  tname = tname,
  idname = idname,
  gname = gname,
  est_method = est_method,
  xformla = xformla,
  control_group = control_group,
  nbins = nbins
)


test_run$plot
test_run$pval
test_run$table
sum(test_run$table[,2])


#do one by hand
#Threshold
# First do some sanity checks

#  Build the bins
bin <- base::as.numeric(base::cut(data_filtered[[yname]],
                                  breaks = nbins,
                                  include.lowest = TRUE,
                                  labels =FALSE))

bin2 <- levels(base::cut(data_filtered[[yname]],
                         breaks = nbins,
                         include.lowest = TRUE,
                         labels =NULL))


level_bin <- base::as.numeric(base::sub("[^,]*,([^]]*)\\]",
                                        "\\1",
                                        base::cut(
                                          data_filtered[[yname]],
                                          breaks = nbins,
                                          include.lowest = TRUE
                                        )
) )

thresh <- sort(unique(level_bin))
manual_test <- matrix(NA,
                      nrow = length(thresh),
                      ncol = 3)

manual_test[,1] <- 1:length(thresh)

for(s in 1:length(thresh)){

  if(s==1) {
    data_filtered$lemp_1 <- -((data_filtered$lemp <= thresh[s]))
  }  else {
    data_filtered$lemp_1 <- -((data_filtered$lemp <= thresh[s]) - (data_filtered$lemp < thresh[s-1]))
  }

  sum((data_filtered$lemp_1==-1) - (bin==s))

  data_filtered$lemp_1 <- base::ifelse(
    data_filtered$first.treat <= data_filtered$year,
    0,
    data_filtered$lemp_1
  )

  test_run_manual <- att_gt(
    data = data_filtered,
    yname = "lemp_1",
    tname = "year",
    idname = idname,
    gname = gname,
    est_method = est_method,
    xformla = NULL,
    control_group = control_group
  )

  manual_test[s,2] <- test_run_manual$att
  manual_test[s,3] <- test_run_manual$se
}

ttests = min(manual_test[,2]/manual_test[,3], na.rm = TRUE)
