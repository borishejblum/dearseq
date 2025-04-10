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
#'
#'@examples
#'#generate fake data
#'n <- 1000 #number of genes
#'nr=5 #number of measurements per subject (grouped data)
#'ni=50 #number of subjects
#'r <- nr*ni #number of measurements
#'t <- matrix(rep(1:nr), ni, ncol=1, nrow=r) # the variable to be tested
#'sigma <- 0.5
#'x <- matrix(1, ncol=1, nrow=r) #no covariates only intercept
#'y.tilde <- rnorm(r, sd = sigma)
#'y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
#'       matrix(rep(y.tilde, n), ncol=n, nrow=r))
#'
#'#Run dear_seq()
#'res_genes <- dear_seq(exprmat=y, covariates=x, variables2test=t,
#'                    sample_group=rep(1:ni, each=nr),
#'                    which_test = "asymptotic",
#'                    which_weights='none', preprocessed=TRUE)
#'
#'#Plot
#'plot_ord_pvals(res_genes$pvals$rawPval)

plot_ord_pvals <- function(pvals, signif_threshold = 0.05){

  df_plot <- data.frame("y" = sort(pvals, na.last = NA), "x" = seq_len(length(pvals[!is.na(pvals)])))

  t <- seq_len(nrow(df_plot))
  s <- (t/length(pvals))*signif_threshold

  ggp <-
    ggplot(data = df_plot, aes_string(x = "x")) +
    scale_y_log10() + annotation_logticks(sides = "l") +
    geom_ribbon(aes(ymin = min(s, .data$y), ymax = s), fill = viridis(4)[3], alpha = 0.1) +
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
