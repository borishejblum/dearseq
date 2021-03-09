#'Plot method for dearseq objects 
#'
#'@param x an object of class \code{dear_seq}
#'
#'@param signif_threshold a value between \code{0} and {1} specifying the 
#'nominal significance threshold. Default is \code{0.05}.
#'
#'@param ... further arguments
#'
#'
#'@author Boris Hejblum
#'
#'@return a \code{\link[ggplot2]{ggplot}} object
#'
#'@import patchwork
#'
#'@export

plot.dearseq <- function(x, signif_threshold = 0.05, ...){
  
  p1 <- plot_hist_pvals(x$pvals$rawPval)
  p2 <- plot_ord_pvals(x$pvals$rawPval, signif_threshold = signif_threshold)
  pf <- p1 +p2
  if(x$weight_object$plot_utilities$method != "none"){
    p3 <- plot_weights(x$weight_object)
    pf <- (p1 + p2) / p3
  }
  return(pf)
}
