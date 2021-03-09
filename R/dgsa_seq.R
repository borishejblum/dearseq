#'Time-course Gene Set Analysis
#'
#'Wrapper function for performing gene set analysis of (potentially
#'longitudinal) RNA-seq data
#'
#'@param exprmat a numeric matrix of size \code{G x n} containing the raw
#'RNA-seq counts or preprocessed expressions from \code{n} samples for \code{G}
#'genes. Default is \code{NULL}, in which case \code{object} must not be
#'\code{NULL}.
#'
#'@param object an object that can be either an
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
#'@param weights_var2test_condi a logical flag indicating whether
#'heteroscedasticity weights computation should be conditional on both the
#'variable(s) to be tested \code{phi} and on covariate(s) \code{x}, or on
#'\code{x} alone. Default is \code{TRUE} in which case conditional means are
#'estimated conditionally on both \code{x} and \code{phi}.
#'
#'@param genesets Can be either:\itemize{
#'\item a \code{vector}
#'\item a \code{list}
#'\item a \code{BiocSet} object
#'}
#'Can be a vector of index or subscripts that defines which
#'rows of \code{y} constitute the investigated gene set (when only 1 gene
#'set is being tested).
#'
#'Can also be a \code{list} of index (or \code{rownames} of \code{y}) when
#'several gene sets are tested at once, such as the first element of a
#'\code{\link[GSA:GSA.read.gmt]{gmt}} object. 
#'
#'Finally, can also be a \code{\link[BiocSet:BiocSet-class]{BiocSet}} object
#'
#'If \code{NULL}, then gene-wise p-values are returned.
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
#'performed). Default is \code{'loclin'}.
#'See \code{\link{sp_weights}} and \code{\link{voom_weights}} for details.
#'
#'@param which_test a character string indicating which method to use to
#'approximate the variance component score test, either \code{'permutation'} or
#'\code{'asymptotic'}. Default is \code{'permutation'}.
#'
#'@param n_perm the number of perturbations. Default is \code{1000}.
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
#'observation-level. Default is \code{TRUE}, and weights are then estimated at
#'the gene-level.
#'
#'@param bw a character string indicating the smoothing bandwidth selection
#'method to use. See \code{\link[stats]{bandwidth}} for details. Possible values
#' are \code{'ucv'}, \code{'SJ'}, \code{'bcv'}, \code{'nrd'} or \code{'nrd0'}
#'
#'@param kernel a character string indicating which kernel should be used.
#'Possibilities are \code{'gaussian'}, \code{'epanechnikov'},
#'\code{'rectangular'}, \code{'triangular'}, \code{'biweight'},
#'\code{'tricube'}, \code{'cosine'}, \code{'optcosine'}. Default is
#'\code{'gaussian'} (NB: \code{'tricube'} kernel
#'corresponds to the loess method).
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
#'\code{'hochberg'}, \code{'hommel'}, \code{'bonferroni'} or \code{'BY'}
#'(for Benjamini-Yekutieli procedure).
#'
#'@param lowess_span smoother span for the lowess function, between 0 and 1.
#'This gives the proportion of points in the plot which influence the smooth at
#'each value. Larger values give more smoothness. Only used if
#'\code{which_weights} is \code{'voom'}. Default is \code{0.5}.
#'
#'@param R library size (optional, important to provide if
#'\code{preprocessed = TRUE}). Default is \code{NULL}
#'
#'@param adaptive a logical flag indicating whether adaptive permutation should 
#'be performed. Default is \code{TRUE}
#'
#'@param max_adaptive The maximum number of permutations considered.
#'Default is \code{64000}
#'
#'@param homogen_traj a logical flag indicating whether trajectories should be
#'considered homogeneous. Default is \code{FALSE} in which case trajectories are
#'not only tested for trend, but also for heterogeneity.
#'
#'@param na.rm_gsaseq logical: should missing values in \code{y} (including
#'\code{NA} and \code{NaN}) be omitted from the calculations?
#'Default is \code{TRUE}.
#'
#'@param verbose logical: should informative messages be printed during the
#'computation? Default is \code{TRUE}.
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
#'   argument defining the gene sets of interest (\code{NULL} for gene-wise t
#'   esting).
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
#'@references Agniel D & Hejblum BP (2017). Variance component score test for
#'time-course gene set analysis of longitudinal RNA-seq data,
#'\emph{Biostatistics}, 18(4):589-604.
#'\href{https://doi.org/10.1093/biostatistics/kxx005}{10.1093/biostatistics/kxx005}.
#'\href{https://arxiv.org/abs/1605.02351}{arXiv:1605.02351}.
#'
#'@references Law, C. W., Chen, Y., Shi, W., & Smyth, G. K. (2014). voom:
#'Precision weights unlock linear model analysis tools for RNA-seq read counts.
#'\emph{Genome Biology}, 15(2), R29.
#'
#'@importFrom stats p.adjust as.formula model.matrix
#'@importFrom matrixStats rowVars
#'@importFrom methods is
#'
#'@examples
#'
#'nsims <- 2 #100
#'res_quant <- list()
#'for(i in 1:2){
#'  n <- 2000#0
#'  nr <- 3
#'  r <- nr*20 #4*nr#100*nr
#'  t <- matrix(rep(1:nr), r/nr, ncol=1, nrow=r)
#'  sigma <- 0.4
#'  b0 <- 1
#'
#'  #under the null:
#'  b1 <- 0
#'
#'  y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
#'  y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
#'         matrix(rep(y.tilde, n), ncol=n, nrow=r))
#'  x <- matrix(1, ncol=1, nrow=r)
#'
#'  #run test
#'  res <- dgsa_seq(exprmat = y, covariates = x, variables2test = t,
#'                 genesets=lapply(0:9, function(x){x*10+(1:10)}),
#'                 cov_variables2test_eff = matrix(1),
#'                 sample_group = rep(1:(r/nr), each=nr),
#'                 which_test='asymptotic',
#'                 which_weights='none', preprocessed=TRUE)
#'  res_genes <- dgsa_seq(exprmat = y, covariates = x,
#'                       variables2test = cbind(t),#, rnorm(r)), #t^2
#'                       genesets = NULL,
#'                       cov_variables2test_eff = diag(1),
#'                       sample_group = rep(1:(r/nr), each=nr),
#'                       which_test = 'asymptotic',
#'                       which_weights = 'none', preprocessed = TRUE)
#'  length(res_genes$pvals[, 'rawPval'])
#'  quantile(res_genes$pvals[, 'rawPval'])
#'  res_quant[[i]] <- res_genes$pvals[, 'rawPval']
#'}
#'
#'
#'#round(rowMeans(vapply(res_quant, FUN = quantile, FUN.VALUE = rep(1.1, 5)), 3)
#'#plot(density(unlist(res_quant)))
#'#mean(unlist(res_quant)<0.05)
#'
#'if(interactive()){
#'res_genes <- dgsa_seq(exprmat = y, covariates = x, variables2test = t,
#'                     genesets = NULL,
#'                     cov_variables2test_eff = matrix(1),
#'                     sample_group = rep(1:(r/nr), each=nr),
#'                     which_test = 'permutation',
#'                     which_weights = 'none', preprocessed = TRUE,
#'                     n_perm = 1000, parallel_comp = FALSE)
#'
#'mean(res_genes$pvals$rawPval < 0.05)
#'summary(res_genes$pvals$adjPval)
#'}
#'@export
dgsa_seq <- function(exprmat = NULL, object = NULL,
                     covariates = NULL,
                     variables2test,
                     weights_var2test_condi = TRUE,
                     genesets,
                     sample_group = NULL,
                     cov_variables2test_eff = NULL,
                     which_test = c("permutation", "asymptotic"),
                     which_weights = c("loclin", "voom", "none"),
                     n_perm = 1000, progressbar = TRUE, parallel_comp = TRUE,
                     nb_cores = parallel::detectCores() - 1,
                     preprocessed = FALSE,
                     gene_based_weights = TRUE,
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
                     na.rm_gsaseq = TRUE,
                     verbose = TRUE) {
    
    if(!is.null(object) & !is.null(exprmat)){
        stop("only one of 'object' or 'exprmat' should be specified")
    }
    if(is.null(object) & is.null(exprmat)){
        stop("One of either 'object' or 'exprmat' should be specified")
    }
    stopifnot(!is.null(variables2test))
    
    if(!is.null(object)){
        
        stopifnot(is.character(covariates) | is.null(covariates))
        stopifnot(is.character(variables2test))
        
        if(is(object, "DGEList")){
            stopifnot(!is.null(object$samples))
            stopifnot(nrow(object$samples)==ncol(object$counts))
            y <- object$counts
            design_df <- as.data.frame(object$samples[, c(covariates, variables2test), drop=FALSE])
        }else if(is(object, "DESeqDataSet")){
            if(!requireNamespace("DESeq2", quietly = TRUE)){
                stop("DESeq2 package required but is not available")
            }
            if(!requireNamespace("SummarizedExperiment", quietly = TRUE)){
                stop("SummarizedExperiment package required but not available")
            }
            stopifnot(!is.null(SummarizedExperiment::colData(object)))
            stopifnot(nrow(SummarizedExperiment::colData(object)) ==
                          DESeq2::counts(object))
            y <- DESeq2::counts(object)
            design_df <- as.data.frame(SummarizedExperiment::colData(object)[, c(covariates, variables2test), drop=FALSE])
        }else if(is(object, "ExpressionSet")){
            if(!requireNamespace("Biobase",quietly=TRUE)){
                stop("Biobase package required but not available")
            }
            stopifnot(!is.null(Biobase::phenoData(object)))
            stopifnot(nrow(Biobase::phenoData(object)) ==
                          ncol(Biobase::exprs(object)))
            y <- Biobase::exprs(object)
            design_df <- as.data.frame(Biobase::phenoData(object)[, c(covariates, variables2test), drop=FALSE])
        }else if(is(object, "SummarizedExperiment")){
            #Note: DESeqDataSet are SummarizedExperiments
            if(!requireNamespace("SummarizedExperiment", quietly = TRUE)){
                stop("SummarizedExperiment package required but is not available")
            }
            y <- SummarizedExperiment::assay(object)
            design_df <- as.data.frame(SummarizedExperiment::colData(object)[, c(covariates, variables2test), drop=FALSE])
        }else{
            stop("'object' is neither a 'DGEList', nor a 'DESeqDataSet',",
                 "nor an 'ExpressionSet'.\n",
                 "Consider specifying 'exprmat' as a matrix instead...")
        }
        
        variables2test_formula <- stats::as.formula(paste0("~ 1 + ", 
                                                           variables2test, 
                                                           collapse=" + "))
        if(is.null(covariates)){
            covariates_formula <- stats::as.formula("~1")
        }else{
            covariates_formula <- stats::as.formula(paste0("~ 1 + ", 
                                                           covariates, 
                                                           collapse=" + "))
        }
        
        x <- stats::model.matrix(covariates_formula, 
                                 data = droplevels(design_df))
        phi <- stats::model.matrix(variables2test_formula, 
                                   data = droplevels(design_df))[, -1, 
                                                                 drop=FALSE]
    }else{
        if(is.null(covariates)){
            covariates <- matrix(1, ncol=1, nrow=nrow(variables2test))
        }
        y <- exprmat
        x <- covariates
        phi <- variables2test
    }
    
    stopifnot(is.matrix(y))
    stopifnot(is.matrix(x) | is.data.frame(x))
    stopifnot(is.matrix(phi) | is.data.frame(phi))
    
    if (sum(is.na(y)) > 1 & na.rm_gsaseq) {
        warning("'y' contains", sum(is.na(y)), "NA values. ",
                "\nCurrently they are ignored in the computations but ",
                "you should think carefully about where do those NA/NaN ",
                "come from...\nIf you don't want to ignore those NA/NaN ",
                "values, set the 'na.rm_gsaseq' argument to 'FALSE' ",
                "(this may lead to errors).")
    }
    
    if(is.null(cov_variables2test_eff)){
        cov_variables2test_eff <- diag(ncol(phi))
    }
    
    # checking for 0 variance genes
    v_g <- matrixStats::rowVars(y)
    if(sum(v_g==0) > 0){
        warning("Removing ", sum(v_g==0), " genes with 0 variance from ",
                "the testing procedure.\n",
                "  Those genes should probably have been removed ",
                "beforehand...")
        y <- y[v_g>0, ]
    }
    
    # normalization if needed
    if (!preprocessed) {
        R <- colSums(y, na.rm = TRUE)
        y_lcpm <- apply(y, MARGIN = 2, function(v) {
            log2((v + 0.5)/(sum(v) + 1) * 10^6)
        })
    } else {
        y_lcpm <- y
    }
    
    if (is.data.frame(x)) {
        message("'x' is a data.frame -> converted to a matrix: ",
                "\n all variables (including factors) are converted ",
                "to numeric...")
        x <- as.matrix(as.data.frame(lapply(x, as.numeric)))
    }
    
    if (is.data.frame(phi)) {
        message("'phi' is a data.frame -> converted to a matrix: ",
                "\n all variables (including factors) are ",
                "converted to numeric... ")
        phi <- as.matrix(as.data.frame(lapply(phi, as.numeric)))
    }
    
    if (det(crossprod(cbind(x, phi))) == 0) {
        stop("crossprod(cbind(x, phi)) cannot be inversed. 'x' and ",
             "'phi' are likely colinear...")
    }
    
    if (length(padjust_methods) > 1) {
        padjust_methods <- padjust_methods[1]
    }
    stopifnot(padjust_methods %in% c("BH", "BY", "holm", "hochberg",
                                     "hommel", "bonferroni"))
    
    if (length(which_weights) > 1) {
        which_weights <- which_weights[1]
    }
    stopifnot(which_weights %in% c("loclin", "voom", "none"))
    
    if (length(which_test) > 1) {
        which_test <- which_test[1]
    }
    stopifnot(which_test %in% c("asymptotic", "permutation"))
    
    
    if (which_test == "asymptotic") {
        n_perm <- NA
        
        if (nrow(x) < 10)
            warning("Less than 10 samples: asymptotics likely not ",
                    "reached\nYou should probably run permutation test ",
                    "instead...")
    }
    
    
    
    if (which_test == "permutation") {
        if (is.null(sample_group)) {
            #suppressWarnings(
            N_possible_perms <- factorial(ncol(y_lcpm))
            #)
        } else {
            # suppressWarnings(
            N_possible_perms <- prod(vapply(table(sample_group), factorial,
                                            FUN.VALUE = 1))
            #)
        }
        
        if (n_perm > N_possible_perms) {
            warning("The number of permutations requested 'n_perm' is ",
                    n_perm, "which is larger than the total number of ",
                    "existing permutations ", N_possible_perms,
                    ". Try a lower number for 'n_perm' (currently ",
                    "running with 'nperm=", N_possible_perms, "').")
            n_perm <- N_possible_perms
        }
    }
    
    
    
    # Computing the weights
    if (which_weights != "none" & verbose) {
        message("Computing the weights... ")
    }
    w_full <- switch(which_weights,
                     loclin = sp_weights(y = y_lcpm, x = x, phi = phi,
                                         use_phi = weights_var2test_condi,
                                         preprocessed = TRUE,
                                         gene_based = gene_based_weights,
                                         bw = bw, kernel = kernel, 
                                         exact = exact, transform = transform, 
                                         verbose = verbose,
                                         na.rm = na.rm_gsaseq),
                     voom = voom_weights(y = y_lcpm, 
                                         x = if(weights_var2test_condi) {
                                             cbind(x, phi)
                                         } else {
                                             x
                                         }, preprocessed = TRUE,
                                         lowess_span = lowess_span,
                                         R = R),
                     none = list(
                         "weights" = matrix(1, ncol = ncol(y_lcpm), 
                                            nrow = nrow(y_lcpm),
                                            dimnames = list(rownames(y_lcpm),
                                                            colnames(y_lcpm))),
                         "plot_utilities" = list("method" = "none"))
    )
    if (which_weights != "none" & verbose) {
        message("Done!\n")
    }
    w <- w_full$weights
    
    
    if (is.null(genesets)) {
        if (verbose) {
            message("'genesets' argument not provided => only gene-wise ",
                    "p-values are computed\n")
        }
        if (which_test == "asymptotic") {
            if (is.null(sample_group)) {
                sample_group <- seq_len(nrow(x))
            }
            rawPvals <- vc_test_asym(y = y_lcpm, x = x, indiv = sample_group,
                                     phi = phi, w = w,
                                     Sigma_xi = cov_variables2test_eff,
                                     genewise_pvals = TRUE,
                                     homogen_traj = homogen_traj,
                                     na.rm = na.rm_gsaseq)$gene_pvals
        } else if (which_test == "permutation") {
            if (is.null(sample_group)) {
                sample_group <- rep(1, nrow(x))
            }
            
            # constructing residuals
            if (na.rm_gsaseq) {
                y_lcpm0 <- y_lcpm
                y_lcpm0[is.na(y_lcpm0)] <- 0
                y_lcpm_res <- y_lcpm - t(x %*% solve(crossprod(x)) %*% t(x) %*%
                                             t(y_lcpm0))
            } else {
                y_lcpm_res <- y_lcpm - t(x %*% solve(crossprod(x)) %*% t(x) %*%
                                             t(y_lcpm))
            }
            x_res <- matrix(1, nrow = nrow(x), ncol = 1)
            perm_result <- vc_test_perm(y = y_lcpm_res, x = x_res,
                                        indiv = sample_group, phi = phi, w = w,
                                        Sigma_xi = cov_variables2test_eff,
                                        n_perm = n_perm,
                                        progressbar = progressbar,
                                        parallel_comp = parallel_comp,
                                        nb_cores = nb_cores,
                                        genewise_pvals = TRUE,
                                        adaptive = adaptive, max_adaptive = max_adaptive,
                                        homogen_traj = homogen_traj,
                                        na.rm = na.rm_gsaseq)
            rawPvals <- perm_result$gene_pvals
        }
        
        pvals <- data.frame(rawPval = rawPvals,
                            adjPval = stats::p.adjust(rawPvals, padjust_methods)
        )
        
        if (!is.null(rownames(y_lcpm))) {
            rownames(pvals) <- rownames(y_lcpm)
        }
    } else {
        if(is(genesets, "BiocSet")){
            genesets <- BiocSet::es_elementset(genesets)
            genesets_names <- unique(genesets$set)
            genesets <- lapply(X  = unique(genesets$set), 
                               FUN = function(x){
                                   genesets[genesets$set == x,]$element
                               }
            )
            names(genesets) <- genesets_names
            
        }
        if (!is.list(genesets)) {
            if (!is.vector(genesets)) {
                stop("'genesets' argument provided but is neither a list ",
                     ", a vector, nor a BiocSet object")
            }
            
            if (is(genesets, "character")) {
                if (is.null(rownames(y_lcpm))) {
                    stop("Gene sets specified as character but no rownames",
                         "available for the expression matrix")
                }
                gene_names_measured <- rownames(y_lcpm)
                if ((length(intersect(genesets,
                                      gene_names_measured))/length(x)) != 1) {
                    message("Some transcripts in the investigated gene set ",
                            "were not measured:\n-> automatically removing ",
                            "those transcripts from the gene set definition")
                    genesets <- genesets[which(genesets %in% 
                                                   gene_names_measured)]
                }
            }
            padjust_methods <- NA
            genesets <- list(genesets)
        }
        
        if (is(genesets[[1]], "character")) {
            if (is.null(rownames(y_lcpm))) {
                stop("Gene sets specified as character but no rownames ",
                     "available for the expression matrix")
            }
            gene_names_measured <- rownames(y_lcpm)
            prop_meas <- vapply(genesets, function(x) {
                length(intersect(x, gene_names_measured))/length(x)
            }, FUN.VALUE = 1)
            if (sum(prop_meas) != length(prop_meas)) {
                warning("Some transcripts in the investigated gene sets were ",
                        "not measured:\nremoving those transcripts from the ",
                        "gene set definition...")
                genesets <- lapply(genesets, function(x) {
                    x[which(x %in% gene_names_measured)]
                })
            }
        }
        
        if (which_test == "asymptotic") {
            if (is.null(sample_group)) {
                sample_group <- seq_len(nrow(x))
            }
            rawPvals <- vapply(seq_along(genesets), FUN = function(i_gs) {
                gs <- genesets[[i_gs]]
                e <- tryCatch(y_lcpm[gs, 1, drop = FALSE], 
                              error=function(cond){return(NULL)})
                if (length(e) < 1) {
                    warning("Gene set ", i_gs, " contains 0 measured ",
                            "transcript: associated p-value cannot ",
                            "be computed")
                    NA
                } else {
                    vc_test_asym(y = y_lcpm[gs, , drop = FALSE], x = x,
                                 indiv = sample_group, phi = phi,
                                 w = w[gs, , drop = FALSE],
                                 Sigma_xi = cov_variables2test_eff,
                                 genewise_pvals = FALSE,
                                 homogen_traj = homogen_traj,
                                 na.rm = na.rm_gsaseq
                    )$set_pval
                }
            }, FUN.VALUE = 0.5)
        } else if (which_test == "permutation") {
            if (is.null(sample_group)) {
                sample_group <- rep(1, nrow(x))
            }
            
            if (na.rm_gsaseq) {
                y_lcpm0 <- y_lcpm
                y_lcpm0[is.na(y_lcpm0)] <- 0
                y_lcpm_res <- y_lcpm - t(x %*% solve(crossprod(x)) %*% t(x) %*%
                                             t(y_lcpm0))
            } else {
                y_lcpm_res <- y_lcpm - t(x %*% solve(crossprod(x)) %*% t(x) %*%
                                             t(y_lcpm))
            }
            
            x_res <- matrix(1, nrow = nrow(x), ncol = 1)
            rawPvals <- vapply(seq_along(genesets), FUN = function(i_gs) {
                gs <- genesets[[i_gs]]
                e <- tryCatch(y_lcpm[gs, 1, drop = FALSE], 
                              error=function(cond){return(NULL)})
                if (length(e) < 1) {
                    warning("Gene set ", i_gs, " contains 0 measured ",
                            "transcript: associated p-value cannot be computed")
                    NA
                } else {
                    vc_test_perm(y = y_lcpm[gs, , drop = FALSE], x = x, 
                                 indiv = sample_group,
                                 phi = phi, w = w[gs, , drop = FALSE],
                                 Sigma_xi = cov_variables2test_eff,
                                 n_perm = n_perm,
                                 progressbar = progressbar,
                                 parallel_comp = parallel_comp,
                                 nb_cores = nb_cores,
                                 genewise_pvals = FALSE,
                                 adaptive = adaptive, max_adaptive = max_adaptive,
                                 homogen_traj = homogen_traj,
                                 na.rm = na.rm_gsaseq
                    )$set_pval
                }
            }, FUN.VALUE = 0.5)
        }
        
        if(!is.na(padjust_methods)){
            pvals <- data.frame(rawPval = rawPvals,
                                adjPval = stats::p.adjust(rawPvals, 
                                                          padjust_methods)
            )
        }else{
            pvals <- data.frame(rawPval = rawPvals, adjPval = NA)
        }
        if (!is.null(names(genesets))) {
            rownames(pvals) <- names(genesets)
        }
        
    }
    if(is.null(genesets)){
        ans_final <- list(which_test = which_test, preprocessed = preprocessed,
                          n_perm = n_perm, pvals = pvals, precision_weights = w, 
                          weight_object = w_full
        )
    }else{
        ans_final <- list(which_test = which_test, preprocessed = preprocessed,
                          n_perm = n_perm, genesets = genesets, pvals = pvals, 
                          precision_weights = lapply(genesets, 
                                                     FUN = function(gs){tryCatch(w[gs, , drop = FALSE], 
                                                                                 error=function(cond){return(NA)})
                                                     }
                          ), 
                          weight_object = w_full
        )
    }
    
    class(ans_final) <- "dearseq"
    return(ans_final)
    
}
