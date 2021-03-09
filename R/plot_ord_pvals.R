#'Plot of gene-wise p-values
#'
#'This function prints the sorted exact p-values along with the Benjamini-Hochberg
#'limit and the 5% threshold
#'
#'@param pvals a vector of length \code{n} containing the raw p-values for
#'each gene
#'
#'@param signif_threshold a value between \code{0} and {1} specifying the 
#'nominal significance threshold. Default is \code{0.05}.
#'
#'
#'@return a plot of sorted gene-wise p-values
#'@import viridisLite ggplot2
#'@export

plot_ord_pvals <- function(pvals, signif_threshold = 0.05){
  
  df_plot <- data.frame("y" = sort(pvals), "x" = c(1:length(pvals)))
  
  t <- c(1:nrow(df_plot))
  s <- (t/length(pvals))*signif_threshold
  
  ggp <- 
    ggplot(data = df_plot, aes_string(x = "x")) + 
    scale_y_log10() + annotation_logticks(sides = "l") +
    geom_ribbon(aes(ymin = min(.data$y), ymax = s), fill = viridis(4)[3], alpha = 0.1) +
    geom_point(aes(y = .data$y, color = "p-values"), size = 0.5, alpha=0.4) +
    geom_line(aes(y = s, color = "B-H significance\nthreshold"), size = 0.5) +
    geom_line(aes(y = signif_threshold, color = paste0(signif_threshold*100, "%")), linetype = 4) +
    scale_color_manual(name = "", breaks = c("p-values", "B-H significance\nthreshold", paste0(signif_threshold*100, "%")),
                       values = c("black", viridis(4)[3], "red")) +
    guides(color = guide_legend(override.aes = list(linetype = c(0,1,4), pch = c(1,NA,NA), size = 1, alpha = 1))) +
    xlab("Descending rank") + 
    ylab("Raw p-value (log10 scale)") +
    theme_bw() +
    ggtitle("FDR significance", subtitle="Benjamini-Hochberg correction vs Ranked raw p-values")
  
  return(ggp)
}
