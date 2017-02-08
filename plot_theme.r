myPlotTheme <- function() {
  return(theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=20),
    axis.title=element_text(size=22),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size=18),
    legend.title = element_text(size=20))) 
}
