#'Differential expression analyis of RNA-seq data through a variance component
#'test
#'
#'Wrapper function for gene-by-gene association testing of RNA-seq data
#'
#'@param exprmat a numeric matrix of size \code{G x n} containing the raw
#'RNA-seq counts or preprocessed expressions from \code{n} samples for \code{G}
#'genes. Default is \code{NULL}, in which case \code{object} must not be
#'\code{NULL}.
#'
#'@param object an object that can be either a
#'\code{\link[SummarizedExperiment:RangedSummarizedExperiment-class]{SummarizedExperiment}},
#'an \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}}, a
#'\code{\link[DESeq2:DESeqDataSet]{DESeqDataSet}}, or a
#'\code{\link[edgeR:DGEList]{DGEList}}.
#'Default is \code{NULL}, in which case \code{exprmat} must not be
#'\code{NULL}.
#'
#'@param covariates \itemize{
#'\item If \code{exprmat} is specified as a matrix:
#'then \code{covariates} must be a numeric matrix of size \code{n x p}
#'containing the model covariates for \code{n} samples (design matrix).
#'Usually, its first column is the intercept (full of \code{1}s).
#'\item If \code{object} is specified: then \code{covariates} must be a
#'character vector of length \code{p} containing the colnames of the
#'design matrix given in \code{object}.
#'} If \code{covariates} is \code{NULL} (the default), then it is just the 
#'intercept.
#'
#'@param variables2test \itemize{
#'\item If \code{exprmat} is specified as a matrix:
#'a numeric design matrix of size \code{n x K} containing
#'the \code{K} variables to be tested.
#'\item If \code{object} is specified: then \code{variables2test} must be a
#'character vector of length \code{K} containing the colnames of the
#'design matrix given in \code{object}.
#'}
#'
#'@param weights_var2test_condi  a logical flag indicating whether
#'heteroscedasticity weights computation should be conditional on both the
#'variables to be tested \code{variables2test} and on the \code{covariates},
#'or on \code{covariates} alone. Default is \code{TRUE} in which case
#'conditional means are estimated conditionally on both \code{variables2test}
#'and \code{covariates}.
#'
#'@param sample_group a vector of length \code{n} indicating whether the samples
#'should be grouped (e.g. paired samples or longitudinal data). Coerced
#'to be a \code{factor}. Default is \code{NULL} in which case no grouping is
#'performed.
#'
#'@param cov_variables2test_eff a matrix of size \code{K x K} containing the
#'covariance matrix of the \code{K} random effects. Only used if
#'\code{homogen_traj} is \code{FALSE}. Default assume diagonal correlation
#'matrix, i.e. independence of random effects.
#'
#'@param which_weights a character string indicating which method to use to
#'estimate the mean-variance relationship weights. Possibilities are
#'\code{'loclin'}, \code{'voom'} or \code{'none'} (in which case no weighting is
#' performed). Default is \code{'loclin'}. See \code{\link{sp_weights}} and
#' \code{\link{voom_weights}} for details.
#'
#'@param which_test a character string indicating which method to use to
#'approximate the variance component score test, either \code{'permutation'} or
#'\code{'asymptotic'}. Default is \code{'permutation'}.
#'
#'@param n_perm the number of perturbations. Default is \code{1000}
#'
#'@param progressbar logical indicating wether a progressBar should be displayed
#'when computing permutations (only in interactive mode).
#'
#'@param parallel_comp a logical flag indicating whether parallel computation
#'should be enabled. Only Linux and MacOS are supported, this is ignored on
#'Windows. Default is \code{TRUE}.
#'
#'@param nb_cores an integer indicating the number of cores to be used when
#'\code{parallel_comp} is \code{TRUE}.
#'Only Linux and MacOS are supported, this is ignored on Windows.
#'Default is \code{parallel::detectCores() - 1}.
#'
#'@param preprocessed a logical flag indicating whether the expression data have
#'already been preprocessed (e.g. log2 transformed). Default is \code{FALSE}, in
#'which case \code{y} is assumed to contain raw counts and is normalized into
#'log(counts) per million.
#'
#'@param gene_based_weights a logical flag used for \code{'loclin'} weights,
#'indicating whether to estimate weights at the gene-level, or rather at the
#'observation-level. Default is \code{FALSE}, which is what it should be for
#'gene-wise analysis.
#'
#'@param bw a character string indicating the smoothing bandwidth selection
#'method to use. See \code{\link[stats]{bandwidth}} for details. Possible values
#'are \code{'ucv'}, \code{'SJ'}, \code{'bcv'}, \code{'nrd'} or \code{'nrd0'}.
#'
#'@param kernel a character string indicating which kernel should be used.
#'Possibilities are \code{'gaussian'}, \code{'epanechnikov'},
#'\code{'rectangular'}, \code{'triangular'}, \code{'biweight'},
#'\code{'tricube'}, \code{'cosine'}, \code{'optcosine'}. Default is
#'\code{'gaussian'} (NB: \code{'tricube'} kernel corresponds to the loess
#'method).
#'
#'@param exact a logical flag indicating whether the non-parametric weights
#'accounting for the mean-variance relationship should be computed exactly or
#'extrapolated from the interpolation of local regression of the mean against
#'the variance. Default is \code{FALSE}, which uses interpolation (faster
#'computation).
#'
#'@param transform a logical flag used for \code{'loclin'} weights, indicating
#'whether values should be transformed to uniform for the purpose of local
#'linear smoothing. This may be helpful if tail observations are sparse and the
#'specified bandwidth gives suboptimal performance there. Default is
#'\code{TRUE}.
#'
#'@param padjust_methods multiple testing correction method used if
#'\code{genesets} is a list. Default is 'BH', i.e. Benjamini-Hochberg procedure
#'for controlling the FDR. Other possibilities are: \code{'holm'},
#'\code{'hochberg'}, \code{'hommel'}, \code{'bonferroni'} or \code{'BY'} (for
#'Benjamini-Yekutieli procedure).
#'
#'@param lowess_span smoother span for the lowess function, between 0 and 1.
#'This gives the proportion of points in the plot which influence the smooth at
#'each value. Larger values give more smoothness. Only used if
#'\code{which_weights} is \code{'voom'}. Default is \code{0.5}.
#'
#'@param R library.size (optional, important to provide if
#'\code{preprocessed = TRUE}). Default is \code{NULL}
#'
#'@param adaptive a logical flag indicating whether adaptive permutation should 
#'be performed. Default is \code{TRUE}
#'
#'@param max_adaptive The maximum number of permutations considered.
#'Default is \code{64000}
#'
#'@param homogen_traj a logical flag indicating whether trajectories should be
#'considered homogeneous. Default is \code{FALSE} in which case trajectories
#'are not only tested for trend, but also for heterogeneity.
#'
#'@param na.rm_dearseq logical: should missing values in \code{y} (including
#'\code{NA} and \code{NaN}) be omitted from the calculations?
#'Default is \code{FALSE}.
#'
#'
#'@return A list with the following elements:\itemize{
#'   \item \code{which_test}: a character string carrying forward the value of
#'   the '\code{which_test}' argument indicating which test was perform (either
#'   'asymptotic' or 'permutation').
#'   \item \code{preprocessed}: a logical flag carrying forward the value of the
#'   '\code{preprocessed}' argument indicating whether the expression data were
#'   already preprocessed, or were provided as raw counts and transformed into
#'   log-counts per million.
#'   \item \code{n_perm}: an integer carrying forward the value of the
#'   '\code{n_perm}' argument indicating the number of perturbations performed
#'   (\code{NA} if asymptotic test was performed).
#'   \item \code{genesets}: carrying forward the value of the '\code{genesets}'
#'   argument defining the gene sets of interest (\code{NULL} for gene-wise
#'   testing).
#'   \item \code{pval}: computed p-values. A \code{data.frame} with one raw for
#'   each each gene set, or for each gene if \code{genesets} argument is
#'   \code{NULL}, and with 2 columns: the first one '\code{rawPval}' contains
#'   the raw p-values, the second one contains the FDR adjusted p-values
#'   (according to the '\code{padjust_methods}' argument) and is named
#'   '\code{adjPval}'.
#' }
#'
#'@seealso \code{\link{sp_weights}} \code{\link{vc_test_perm}}
#'\code{\link{vc_test_asym}} \code{\link{p.adjust}}
#'
#'@references Gauthier M, Agniel D, Thiébaut R & Hejblum BP (2019).
#'dearseq: a variance component score test for RNA-Seq differential analysis
#'that effectively controls the false discovery rate, *bioRxiv* 635714.
#'[DOI: 10.1101/635714v1](https://www.biorxiv.org/content/10.1101/635714v1).
#'
#'@examples
#'
#'#Monte-Carlo estimation of the proportion of DE genes over `nsims` simulations under the null
#'
#'#number of runs
#'nsims <- 2 #100 
#'res <- numeric(nsims)

#'for(i in 1:nsims){
#'  n <- 1000 #number of genes
#'  nr=5 #number of measurements per subject (grouped data)
#'  ni=50 #number of subjects
#'  r <- nr*ni #number of measurements
#'  t <- matrix(rep(1:nr), ni, ncol=1, nrow=r) # the variable to be tested 
#'  sigma <- 0.5
#'  b0 <- 1
#'  
#'  #under the null:
#'  b1 <- 0
#'  
#'  #create the matrix of gene expression
#'  y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
#'  y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
#'           matrix(rep(y.tilde, n), ncol=n, nrow=r))
#'  
#'  #no covariates
#'  x <- matrix(1, ncol=1, nrow=r)
#'  
#'  #run test
#'  #asymptotic test with preprocessed grouped data
#'  res_genes <- dear_seq(exprmat=y, covariates=x, variables2test=t,
#'                        sample_group=rep(1:ni, each=nr),
#'                        which_test='asymptotic',
#'                       which_weights='none', preprocessed=TRUE)
#' 
#'  #proportion of raw p-values>0.05
#'  mean(res_genes$pvals[, 'rawPval']>0.05)
#'  
#'  #quantiles of raw p-values
#'  quantile(res_genes$pvals[, 'rawPval'])
#'  
#'  #proportion of raw p-values<0.05 i.e. proportion of DE genes
#'  res[i] <- mean(res_genes$pvals[, 'rawPval']<0.05)
#'  message(i)
#'}
#'
#'#results
#'mean(res)
#'
#'if(interactive()){
#'b0 <- 1
#'#under the null:
#'b1 <- 0
#'
#'#create the matrix of gene expression
#'y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
#'y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
#'       matrix(rep(y.tilde, n), ncol=n, nrow=r))
#'
#'#run test
#'#asymptotic test with preprocessed grouped data
#'res_genes <- dear_seq(exprmat=y, covariates=x, variables2test=t,
#'                    sample_group=rep(1:ni, each=nr),
#'                    which_weights='none', preprocessed=TRUE)
#'
#'#results
#'summary(res_genes$pvals)
#'}
#'@export
dear_seq <- function(exprmat = NULL, object = NULL,
                     covariates =NULL,
                     variables2test,
                     sample_group = NULL,
                     weights_var2test_condi = TRUE,
                     cov_variables2test_eff = NULL,
                     which_test = c("permutation", "asymptotic"),
                     which_weights = c("loclin", "voom", "none"),
                     n_perm = 1000, progressbar = TRUE, parallel_comp = TRUE,
                     nb_cores = parallel::detectCores() - 1,
                     preprocessed = FALSE,
                     gene_based_weights = FALSE,
                     bw = "nrd",
                     kernel = c("gaussian", "epanechnikov", "rectangular",
                                "triangular", "biweight", "tricube", "cosine",
                                "optcosine"),
                     exact = FALSE,
                     transform = TRUE,
                     padjust_methods = c("BH", "BY", "holm", "hochberg",
                                         "hommel", "bonferroni"),
                     lowess_span = 0.5,
                     R=NULL,
                     adaptive = TRUE, max_adaptive = 64000,
                     homogen_traj = FALSE,
                     na.rm_dearseq = TRUE) {

  return(dgsa_seq(exprmat = exprmat, object = object,
                 covariates = covariates,
                 variables2test = variables2test,
                 weights_var2test_condi = weights_var2test_condi,
                 genesets = NULL,
                 sample_group = sample_group,
                 cov_variables2test_eff = cov_variables2test_eff,
                 which_test = which_test,
                 which_weights = which_weights,
                 n_perm = n_perm, progressbar = progressbar,
                 parallel_comp = parallel_comp, nb_cores = nb_cores,
                 preprocessed = preprocessed,
                 gene_based_weights = gene_based_weights,
                 bw = bw,
                 kernel = kernel,
                 exact = exact,
                 transform = transform,
                 padjust_methods = padjust_methods,
                 lowess_span = lowess_span,
                 R = R,
                 adaptive = adaptive, max_adaptive = max_adaptive,
                 homogen_traj = homogen_traj,
                 na.rm_gsaseq = na.rm_dearseq,
                 verbose = FALSE))

}
