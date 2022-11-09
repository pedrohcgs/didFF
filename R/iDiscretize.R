iDiscretize<-function(DF=NULL,
                            idvar="id",
                            yvar="outcome",
                            tvar="timevar",
                            treatmentvar="treatment",
                            weight=NULL,
                            nbins=NULL)
                            {

if (is.null(nbins)){
  bin<-as.numeric(DF[[yvar]])
  level<-as.numeric(DF[[yvar]])
  warning<-as.data.frame(bin) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(no_obs = length(bin))
  min_obs<-min(warning$no_obs)
  if(min_obs<10){message("Some bins have less than 10 observations. Please consider reducing the number of bins using the nbins argument.")}
  }

  else{
   bin <- as.numeric(cut(DF[[yvar]],
                        breaks=nbins,
                        include.lowest=TRUE,
                        labels=FALSE))

  level<-as.numeric( sub("[^,]*,([^]]*)\\]", "\\1",cut(DF[[yvar]],
                                                       breaks=nbins,
                                                       include.lowest=TRUE)) )}

  y <- as.numeric(DF[[yvar]])
  year <- DF[[tvar]]
  D<-DF[[treatmentvar]]
  id<-DF[[idvar]]

  if (is.null(weight)) {
    w<-rep(1, length(y))
  } else {
    w<-DF[[weight]]}


  df1<-data.frame(id,bin,level,y,year,D,w)

  return(df1)
} #Discretize the support of y for density estimation and return a df ready for didFF
