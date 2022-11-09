#' Test if Parallel trends assumption is sensitive to functional form
#'
#' @param DF data.frame object
#' @param yvar Name of Outcome variable in DF
#' @param tvar Name Time variable in DF
#' @param treatmentvar Name of Binary variable in df indicating treatment/control group
#' @param nbins A scalar indicating the Number of bins for the support of Outcome. Default nbins=100
#' @param start_t The Baseline period for DID. Default min(tvar)
#' @param end_t The follow up period for DID. Default max(tvar)
#' @param idvar Name variable indicating group or id in DF
#' @param weight Name weighting variable. Default is NULL
#' @param nboots Number of bootstrap samples for iDensityTest. Default nboots=1000
#' @param seed Starting seed for iDensityTest. Default is seed=0, set seed=NULL for random seed.
#' @param lb_graph Minimun outcome-bin for density estimation. Default lb_graph=NULL
#' @param ub_graph Maximun outcome-bin for density estimation. Default ub_graph=NULL
#'
#' @return A list object "list(plot,table,pval)" containing the plot of the implied density under the null, a table with the estimated and implied densities, and the pval for H0= Implied Density>0.
#' @export
#'
#' @examples
#' set.seed(99)
#' N=10000
#' treat<-rbinom(N,1,0.25)
#' wage<-rlnorm(N, 13, 2)
#' id<-as.character(seq(1:N))
#' t0<-replicate(N,2010)
#' t1<-replicate(N,2020)
#' y0<-wage+rnorm(N,0,100)
#' y1<- y0 + 20*treat + rnorm(N,0,100)
#' panel_id=c(paste(id,t0,sep="_"),paste(id,t1,sep="_"))
#' year<-c(t0,t1)
#' y<-c(y0,y1)
#' D<-c(treat,treat)
#' treated<-c(replicate(N,0),treat)
#' ID<-c(id,id)
#' df<-data.frame(panel_id,ID,year,y,D,treated)
#' df<-df%>%
#'dplyr::filter((0 <= y) & (y <= 500000))
#' didFF(df,"ID","y","year","treated", nbins=100)


didFF<-function(DF=NULL,
                idvar = "id",
                yvar = "output",
                tvar= "timevar",
                treatmentvar = "treatment group",
                weight=NULL,
                nbins=NULL,
                nboots=1000,
                seed=0,
                start_t=NULL,
                end_t=NULL,
                lb_graph = NULL,
                ub_graph = NULL
){


  df1<-iDiscretize(DF,idvar,yvar,tvar,treatmentvar,weight,nbins)

  implied_density_table<-iDensity(df1,start_t,end_t)
  if(is.null(lb_graph)){min<-base::min(implied_density_table$level)}
  else {min<-lb_graph}

  if(is.null(ub_graph)){max<-base::max(implied_density_table$level)}
  else {max<-ub_graph}

  plotTable <- iplotTable(implied_density_table,min,max)

  implied_density_plot <- iDensityPlot(plotTable,start_t,end_t)

  pval <- iDensityTest(DF,idvar,yvar,tvar,treatmentvar,weight,nbins,nboots,seed,start_t,end_t)

  rpval<-round(pval,3)

  H0_text = list("H[0]: 'Implied Density' >= 0")

  if(pval<0.01){
    pval_text= list("p-value <0.01")}
  else{ pval_text=list(paste("p-value =",rpval))}


  plot<-implied_density_plot +
    ggplot2::annotate(geom = 'text',
                      x = base::mean(plotTable$level)+stats::sd(plotTable$implied_density_post),
                      y = base::max(plotTable$implied_density_post),
                      label = H0_text, parse=TRUE,
                      hjust = 0) +
    ggplot2::annotate(geom = 'text',
                      x = base::mean(plotTable$level)+stats::sd(implied_density_table$implied_density_post),
                      y = base::max(plotTable$implied_density_post)-stats::sd(plotTable$implied_density_post)/3,
                      label = pval_text,
                      hjust = 0) +
    ggplot2::xlab(yvar)

  didTest<-list("plot"=plot, "table"=implied_density_table, "pval"=pval)

  return(didTest)


}

