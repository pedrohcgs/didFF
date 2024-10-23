
##Load the subsets of the data from Callaway and Sant'Anna (2021) and Cengiz et. al (2019)


Cengiz_df <- load("data-raw/Cengiz_df.rda")
mw_df <- load("data-raw/mw_df.rda")

# Export the data
usethis::use_data(Cengiz_df, overwrite = TRUE)
usethis::use_data(mw_df, overwrite = TRUE)
