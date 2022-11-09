iplotTable<-function(df,min=NULL,max=NULL){
    df%>%
    dplyr::filter(min <= level, level <= max)%>%
    dplyr::mutate(Outcome = level) %>%
    dplyr::mutate(`Implied Density` = ifelse(implied_density_post < 0,
                                             "Negative", "Non-negative"))}
