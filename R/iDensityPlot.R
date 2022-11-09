iDensityPlot <- function(DF=NULL,
                         start_t=NULL,
                         end_t=NULL){
  plotTable<-DF #data.frame resulting from iDensity()

  if (is.null(start_t)) {
    sy<-sub("_0","",colnames(plotTable)[2]) #Starting year
  } else {
    sy<-start_t
  }
  if (is.null(end_t)) {
    ey<-sub("_0","",colnames(plotTable)[ncol(DF)]) #Ending yearplot1
  } else {
    ey<-end_t
  }




  #Generate basic plot

  plot<-plotTable%>%
    ggplot2::ggplot(ggplot2::aes(x=Outcome,
                        y = implied_density_post,
                        fill = `Implied Density`)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Outcome") +
    ggplot2::ylab("Implied Density")

  #Format plot
  plot_title<-paste("DiD Between",sy,"-",ey)
  plot2 <- plot +

    ggthemes::theme_clean(base_size=12) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::theme(
      # Background
      plot.background = ggplot2::element_blank(),

      # Format legend
      legend.text = ggplot2::element_text(size=10),
      legend.title = ggplot2::element_text(size=10),
      legend.box.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),

      # Set title and axis labels, and format these and tick marks
      plot.title=ggplot2::element_text(size=13, vjust=1.25, hjust = 0.5),
      axis.text.x=ggplot2::element_text(size=10),
      axis.text.y=ggplot2::element_text(size=10),
      axis.title.x=ggplot2::element_text(size=10, vjust=0),
      axis.title.y=ggplot2::element_text(size=10, vjust=1.25),

      # Plot margins
      plot.margin = grid::unit(c(0.35, 0.2, 0.3, 0.35), "cm")
    ) +

    ggplot2::labs(title = plot_title)
  return(plot2)

} #Density PLot
