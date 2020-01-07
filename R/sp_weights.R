#'Non parametric local heteroscedasticity weights
#'
#'Computes precision weights that account for heteroscedasticity in RNA-seq
#'count data based on non-parametric local linear regression estimates.
#'
#'@param y a numeric matrix of size \code{G x n} containing the raw RNA-seq
#'counts or preprocessed expression from \code{n} samples for \code{G} genes.
#'
#'@param x a numeric matrix of size \code{n x p} containing the model
#'covariate(s) from \code{n} samples (design matrix).
#'
#'@param phi a numeric design matrix of size \code{n x K} containing the K
#'variable(s) of interest( e.g. bases of time).
#'
#'@param use_phi a logical flag indicating whether conditional means should be
#'conditioned on \code{phi} and on covariate(s) \code{x}, or on \code{x} alone.
#'Default is \code{TRUE} in which case conditional means are estimated
#'conditionally on both \code{x} and \code{phi}.
#'
#'@param preprocessed a logical flag indicating whether the expression data have
#'already been preprocessed (e.g. log2 transformed). Default is \code{FALSE}, in
#'which case \code{y} is assumed to contain raw counts and is normalized into
#'log(counts) per million.
#'
#'@param gene_based a logical flag indicating whether to estimate weights at the
#'gene-level. Default is \code{FALSE}, when weights will be estimated at the
#'observation-level.
#'
#'@param bw a character string indicating the smoothing bandwidth selection
#'method to use. See \code{\link[stats]{bandwidth}} for details. Possible values
#'are \code{'ucv'}, \code{'SJ'}, \code{'bcv'}, \code{'nrd'} or \code{'nrd0'}.
#'Default is \code{'nrd'}.
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
#'the variance. Default is \code{FALSE}, which uses interpolation (faster).
#'
#'@param transform a logical flag indicating whether values should be
#'transformed to uniform for the purpose of local linear smoothing. This may be
#'helpful if tail observations are sparse and the specified bandwidth gives
#'suboptimal performance there. Default is \code{TRUE}.
#'
#'@param verbose a logical flag indicating whether informative messages are
#'printed during the computation. Default is \code{TRUE}.
#'
#'@param na.rm logical: should missing values (including \code{NA} and
#'\code{NaN}) be omitted from the calculations? Default is \code{FALSE}.
#'
#'@return a list containing the following components:\itemize{
#'\item \code{weights}: a matrix \code{n x G} containing the computed precision 
#'weights 
#'\item \code{plot_utilities}: a list containing the following elements:\itemize{
#'      \item\code{reverse_trans}: a function encoding the reverse function used 
#'      for smoothing the observations before computing the weights
#'      \item\code{method}: the weight computation method (\code{"loclin"})
#'      \item\code{smth}: the vector of the smoothed values computed 
#'      \item\code{gene_based}: a logical indicating whether the computed 
#'      weights are based on average at the gene level or on individual 
#'      observations
#'      \item\code{mu}: the transformed observed counts or averages
#'      \item\code{v}: the observed variability estimates
#' }
#'}
#'
#'@author Boris Hejblum
#'
#'@seealso \code{\link[stats]{bandwidth}} \code{\link{density}}
#'
#'@examples
#'set.seed(123)
#'
#'G <- 10000
#'n <- 12
#'p <- 2
#'y <- sapply(1:n, FUN = function(x){rnbinom(n = G, size = 0.07, mu = 200)})
#'
#'x <- sapply(1:p, FUN = function(x){rnorm(n = n, mean = n, sd = 1)})
#'
#'w <- sp_weights(y, x, use_phi=FALSE, na.rm = TRUE)
#'
#'@import ggplot2
#'@importFrom stats bw.bcv bw.nrd0 bw.nrd bw.SJ bw.ucv dnorm approx sd pnorm
#'qnorm
#'@importFrom KernSmooth locpoly
#'@export


sp_weights <- function(y, x, phi = NULL, use_phi = TRUE, preprocessed = FALSE,
                       gene_based = FALSE,
                       bw = c("nrd", "ucv", "SJ", "nrd0", "bcv"),
                       kernel = c("gaussian", "epanechnikov", "rectangular",
                                  "triangular", "biweight", "tricube", "cosine",
                                  "optcosine"),
                       exact = FALSE, transform = TRUE, verbose = TRUE,
                       na.rm = FALSE) {
    
    
    ## dimensions & validity checks
    
    stopifnot(is.matrix(y))
    stopifnot(is.matrix(x))
    stopifnot(is.null(phi) | is.matrix(phi))
    
    g <- nrow(y)  # the number of genes measured
    n <- ncol(y)  # the number of samples measured
    qq <- ncol(x)  # the number of covariates
    stopifnot(nrow(x) == n)
    if(use_phi){
        stopifnot(nrow(phi) == n)
    }

    # removing genes never observed:
    observed <- which(rowSums(y, na.rm = TRUE) != 0)
    nb_g_sum0 <- length(observed) - g
    if (nb_g_sum0 > 0) {
        warning(nb_g_sum0, " y rows sum to 0 (i.e. are never observed)",
                "and have been removed")
    }
    
    kernel <- match.arg(kernel)
    if (preprocessed) {
        y_lcpm <- t(y[observed, ])
    } else {
        # transforming raw counts to log-counts per million (lcpm)
        y_lcpm <- t(apply(y[observed, ], MARGIN = 2, function(v) {
            log2((v + 0.5)/(sum(v) + 1) * 10^6)
        }))
    }
    N <- length(y_lcpm)
    p <- ncol(y_lcpm)
    
    
    # fitting OLS to the lcpm
    xphi <- if (use_phi) {
        cbind(x, phi)
    } else {
        x
    }
    if (na.rm) {
        y_lcpm0 <- y_lcpm
        y_lcpm0[is.na(y_lcpm0)] <- 0
        B_ols <- solve(crossprod(xphi)) %*% t(xphi) %*% y_lcpm0
    } else {
        B_ols <- solve(crossprod(xphi)) %*% t(xphi) %*% y_lcpm
    }
    mu <- xphi %*% B_ols
    
    sq_err <- (y_lcpm - mu)^2
    lse <- log(sq_err)
    v <- colMeans(sq_err, na.rm = na.rm)
    mu_avg <- colMeans(mu, na.rm = na.rm)
    
    if (gene_based) {
        mu_x <- mu_avg
    } else {
        mu_x <- mu
        mu_x[is.na(y_lcpm)] <- NA
    }
    # transforming if necessary
    if (transform) {
        sd_mu <- sd(mu_x, na.rm = na.rm)
        mean_mu <- mean(mu_x, na.rm = na.rm)
        mu_x <- pnorm((mu_x - mean_mu)/sd_mu)
        reverse_trans <- function(x) {
            qnorm(x) * sd_mu + mean_mu
        }
    } else {
        reverse_trans <- identity
    }
    
    if (is.character(bw)) {
        if (length(bw > 1)) {
            bw <- bw[1]
        }
        if (N < 2) {
            stop("need at least 2 points to select a bandwidth automatically")
        }
        if (!exact) {
            bw <- switch(bw,
                         nrd0 = stats::bw.nrd0(as.vector(mu_x)),
                         nrd = stats::bw.nrd(as.vector(mu_x)),
                         ucv = stats::bw.ucv(as.vector(mu_x)),
                         bcv = stats::bw.bcv(as.vector(mu_x)),
                         SJ = stats::bw.SJ(as.vector(mu_x), method = "ste"),
                         stop("unknown bandwidth rule: 'bw' argument",
                              "must be among 'nrd0', 'nrd', 'ucv',",
                              "'bcv', 'SJ'")
            )
        }
        if (verbose) {
            message("\nBandwith computed.\n")
        }
    }
    
    if (!is.finite(bw)) {
        stop("non-finite 'bw'")
    }
    if (bw <= 0) {
        stop("'bw' is not positive")
    }
    
    # choose kernel ----
    if (kernel == "gaussian") {
        kern_func <- function(x, bw) {
            stats::dnorm(x, sd = bw)
        }
    } else if (kernel == "rectangular") {
        kern_func2 <- function(x, bw) {
            a <- bw * sqrt(3)
            (abs(x) < a) * 0.5/a  #ifelse(abs(x) < a, 0.5/a, 0)
            
        }
    } else if (kernel == "triangular") {
        kern_func <- function(x, bw) {
            h <- bw * sqrt(6)
            ax <- abs(x)
            (ax < h) * ((1 - ax/h)/h)
        }
    } else if (kernel == "tricube") {
        kern_func <- function(x, bw) {
            h <- bw * sqrt(243/35)
            ax <- abs(x)
            (ax < h) * (70/81 * (1 - (ax/h)^3)^3)/h
        }
    } else if (kernel == "epanechnikov") {
        kern_func <- function(x, bw) {
            h <- bw * sqrt(5)
            ax <- abs(x)
            (ax < h) * (3/4 * (1 - (ax/h)^2)/h)
        }
    } else if (kernel == "biweight") {
        kern_func <- function(x, bw) {
            h <- bw * sqrt(7)
            ax <- abs(x)
            (ax < h) * (15/16 * (1 - (ax/h)^2)^2/h)
        }
    } else if (kernel == "cosine") {
        kern_func <- function(x, bw) {
            h <- bw/sqrt(1/3 - 2/pi^2)
            (abs(x) < h) * ((1 + cos(pi * x/h))/(2 * h))
        }
    } else if (kernel == "optcosine") {
        kern_func <- function(x, bw) {
            h <- bw/sqrt(1 - 8/pi^2)
            (abs(x) < h) * (pi/4 * cos(pi * x/(2 * h))/h)
        }
    } else {
        stop("unknown kernel: 'kernel' argument must be among",
             "'gaussian', 'rectangular', 'triangular', 'epanechnikov',",
             "'biweight', 'cosine', 'optcosine'")
    }
    
    
    if (gene_based) {
        
        w <- function(x) {
            x_ctr <- (mu_x - x)
            kernx <- kern_func(x_ctr, bw)
            Sn1 <- kernx * x_ctr
            #Sn2 <- Sn1 * x_ctr
            b <- kernx * (sum(Sn1 * x_ctr) - x_ctr * sum(Sn1))
            l <- b/sum(b)
            sum(l * v)
        }
        
        kern_fit <- NULL
        if (exact) {
            
            message("'exact' is TRUE: the computation may take up to a",
                    "couple minutes...", "\n", "Set 'exact = FALSE' for",
                    "quicker computation of the weights using smoothing\n")
            
            weights <- t(matrix(1/unlist(lapply(as.vector(mu_x), w)), ncol = n,
                                nrow = p, byrow = FALSE))
            if (sum(!is.finite(weights)) > 0) {
                warning("At least 1 non finite weight. ",
                        "Try to increase the bandwith")
            }
        } else {
            kern_fit <- vapply(mu_x, w, FUN.VALUE = 1.1)
            weights <- 1/matrix(kern_fit, nrow = n, ncol = p, byrow = TRUE)
            #NB: byrow=T for genesets (recycling the weights) but F for genewise
            #Alternative approximation:
            #f_interp <- stats::approxfun(x = mu_avg, kern_fit, rule = 2)
            #weights <- 1/apply(mu, 2, f_interp)
        }

    } else {
        if (!na.rm) {
            stop("Cannot compute the weights without ignoring NA/NaN ",
                 "values...\nTry setting 'na.rm' or na.rm_gsaseq' to 'TRUE' ",
                 "to ignore NA/NaN values, but think carefully about where ",
                 "does those NA/NaN come from...")
        } else if (sum(is.na(mu_x)) > 1) {
            mu_x <- mu_x[-which(is.na(mu_x))]
            sq_err <- sq_err[-which(is.na(sq_err))]
            lse <- lse[-which(is.na(lse))]
        }
        smth <- KernSmooth::locpoly(x = c(mu_x), y = c(lse), degree = 1,
                                    kernel = kernel, bandwidth = bw)
        w <- (1/exp(stats::approx(x = reverse_trans(smth$x), y = smth$y,
                                  xout = reverse_trans(mu_x),
                                  rule = 2)$y))
        weights <- matrix(w, nrow = n, ncol = p)
    }
    if(sum(weights<0)>1){
        stop("negative variance weights estimated: please contact the authors",
             "of the package")
    }
    
    colnames(weights) <- colnames(y_lcpm)
    rownames(weights) <- rownames(y_lcpm)
    

    if(gene_based){
        v_out <- v
        smth_out <- kern_fit
    }else{
        v_out <- lse
        smth_out <- smth
    }
    return(list("weights" = t(weights), 
                "plot_utilities" = list("reverse_trans" = reverse_trans,
                                        "method" = "loclin",
                                        "smth" = smth_out,
                                        "gene_based" = gene_based,
                                        "mu" = mu_x,
                                        "v" = v_out
                )
    ))
}

