#'Computes variance component score test statistics for homogeneous trajectory
#'and its permuted distribution
#'
#'This function computes the variance component score test statistics
#'for homogeneous trajectories along with its permuted values for estimating
#'its distribution under the null hypothesis.
#'
#'@keywords internal
#'
#'@param y a numeric matrix of dim \code{g x n} containing the raw or
#'normalized RNA-seq counts for g genes from \code{n} samples.
#'
#'@param x a numeric design matrix of dim \code{n x p} containing the \code{p}
#'covariates to be adjusted for.
#'
#'@param indiv a vector of length \code{n} containing the information for
#'attributing each sample to one of the studied individuals. Coerced
#'to be a \code{factor}.
#'
#'@param phi a numeric design matrix of size \code{n x K} containing the
#'\code{K} longitudinal variables to be tested (typically a vector of time
#'points or functions of time).
#'
#'@param w a vector of length \code{n} containing the weights for the \code{n}
#'samples, corresponding to the inverse of the diagonal of the estimated
#'covariance matrix of y.
#'
#'@param Sigma_xi a matrix of size \code{K x K} containing the covariance matrix
#'of the \code{K} random effects corresponding to \code{phi}.
#'
#'@param na_rm logical: should missing values (including \code{NA} and
#'\code{NaN}) be omitted from the calculations? Default is \code{FALSE}.
#'
#'@param n_perm the number of permutation to perform. Default is \code{1000}.
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
#'Default is \code{parallel::detectCores(logical=FALSE) - 1}.
#'
#'@return A list with the following elements:
#'\itemize{
#'   \item \code{score}: an approximation of the observed set score
#'   \item \code{scores_perm}: a vector containing the permuted set scores
#'   \item \code{gene_scores_unscaled}: approximation of the individual gene
#'   scores
#'   \item \code{gene_scores_unscaled_perm}: a list of approximation of the
#'   permuted individual gene scores
#' }
#'
#'
#'
#'@examples
#'set.seed(123)
#'
#'##generate some fake data
#'########################
#'ng <- 100
#'nindiv <- 30
#'nt <- 5
#'nsample <- nindiv*nt
#'tim <- matrix(rep(1:nt), nindiv, ncol=1, nrow=nsample)
#'tim2 <- tim^2
#'sigma <- 5
#'b0 <- 10
#'
#'#under the null:
#'beta1 <- rnorm(n=ng, 0, sd=0)
#'#under the (heterogen) alternative:
#'beta1 <- rnorm(n=ng, 0, sd=0.1)
#'#under the (homogen) alternative:
#'beta1 <- rnorm(n=ng, 0.06, sd=0)
#'
#'y.tilde <- b0 + rnorm(ng, sd = sigma)
#'y <- t(matrix(rep(y.tilde, nsample), ncol=ng, nrow=nsample, byrow=TRUE) +
#'       matrix(rep(beta1, each=nsample), ncol=ng, nrow=nsample, byrow=FALSE) *
#'            matrix(rep(tim, ng), ncol=ng, nrow=nsample, byrow=FALSE) +
#'       #matrix(rep(beta1, each=nsample), ncol=ng, nrow=nsample, byrow=FALSE) *
#'       #     matrix(rep(tim2, ng), ncol=ng, nrow=nsample, byrow=FALSE) +
#'       matrix(rnorm(ng*nsample, sd = sigma), ncol=ng, nrow=nsample,
#'              byrow=FALSE)
#'       )
#'myindiv <- rep(1:nindiv, each=nt)
#'x <- cbind(1, myindiv/2==floor(myindiv/2))
#'myw <- matrix(rnorm(nsample*ng, sd=0.1), ncol=nsample, nrow=ng)
#'
#'#run test
#'#We only use few permutations (10) to keep example running time low
#'#Otherwise one can use n_perm = 1000
#'score_homogen <- vc_score_h_perm(y, x, phi=tim, indiv=myindiv,
#'                                 w=myw, Sigma_xi=cov(tim), n_perm = 10,
#'                                 parallel_comp = FALSE)
#'score_homogen$score
#'
#'score_heterogen <- vc_score_perm(y, x, phi=tim, indiv=myindiv,
#'                            w=myw, Sigma_xi=cov(tim), n_perm = 10,
#'                            parallel_comp = FALSE)
#'score_heterogen$score
#'
#'scoreTest_homogen <- vc_test_asym(y, x, phi=tim, indiv=rep(1:nindiv, each=nt),
#'                                  w=matrix(1, ncol=ncol(y), nrow=nrow(y)),
#'                                  Sigma_xi=cov(tim), homogen_traj = TRUE)
#'scoreTest_homogen$set_pval
#'scoreTest_heterogen <- vc_test_asym(y, x, phi=tim, indiv=rep(1:nindiv,
#'                                                             each=nt),
#'                                    w=matrix(1, ncol=ncol(y), nrow=nrow(y)),
#'                                    Sigma_xi=cov(tim), homogen_traj = FALSE)
#'scoreTest_heterogen$set_pval
#'
#'@importFrom pbapply pbsapply
#'@importFrom parallel mclapply
#'
#'@export
vc_score_h_perm <- function(y, x, indiv, phi, w,
                            Sigma_xi = diag(ncol(phi)),
                            na_rm = FALSE,
                            n_perm = 1000,
                            progressbar = TRUE,
                            parallel_comp = TRUE,
                            nb_cores = parallel::detectCores(logical=FALSE) - 1) {

    ## validity checks
    if (sum(!is.finite(w)) > 0) {
        stop("At least 1 non-finite weight in 'w'")
    }

    ## dimensions check------

    stopifnot(is.matrix(y))
    stopifnot(is.matrix(x))
    stopifnot(is.matrix(phi))

    g <- nrow(y)  # the number of genes measured
    n <- ncol(y)  # the number of samples measured
    p <- ncol(x)  # the number of covariates
    n_t <- ncol(phi)  # the number of time bases
    stopifnot(nrow(x) == n)
    stopifnot(nrow(w) == g)
    stopifnot(ncol(w) == n)
    stopifnot(nrow(phi) == n)
    stopifnot(length(indiv) == n)


    # the number of random effects
    if (length(Sigma_xi) == 1) {
        K <- 1
        Sigma_xi <- matrix(Sigma_xi, K, K)
    } else {
        K <- nrow(Sigma_xi)
        stopifnot(ncol(Sigma_xi) == K)
    }
    stopifnot(n_t == K)


    ## data formating ------
    indiv <- factor(indiv, ordered = TRUE)
    nb_indiv <- length(levels(indiv))

    y_T <- t(y)

    ## x_tilde_list <- y_tilde_list <- Phi_list <- list()
    ## for (i in 1:nb_indiv) {
    ##     select <- indiv==levels(indiv)[i]
    ##     n_i <- length(which(select))
    ##     x_i <- x[select,]
    ##     y_i <- y_T[select,]
    ##     phi_i <- phi[select,]
    ##     Phi_list[[i]] <- do.call(rbind, replicate(g, phi_i,
    ##                              simplify = FALSE)) #TODO
    ##     x_tilde_list[[i]] <- matrix(data=rep(x_i, each=g), ncol = p) #TODO
    ##     y_tilde_list[[i]] <- matrix(y_i, ncol=1)
    ## }
    ## x_tilde <- do.call(rbind, x_tilde_list)
    ## y_tilde <- do.call(rbind, y_tilde_list)
    ## Phi <- do.call(rbind, Phi_list)
    ## alpha <- solve(t(x_tilde)%*%x_tilde)%*%t(x_tilde)%*%y_tilde
    ## mu_new <- x_tilde %*% alpha
    ## y_mu <- y_tilde - mu_new


    alpha <- solve(crossprod(x)) %*% t(x) %*% rowMeans(y_T, na.rm = na_rm)
    yt_mu <- y_T - do.call(cbind, replicate(g, x %*% alpha, simplify = FALSE))


    ## test statistic computation ------
    sig_xi_sqrt <- (Sigma_xi * diag(K)) %^% (-0.5)
    # sig_xi_sqrt <- (Sigma_xi %^% (-0.5))
    ## xtx_inv <- solve(t(x_tilde) %*% x_tilde)
    ## long_indiv <- rep(indiv, each = g)
    ## q <- matrix(NA, nrow=nb_indiv, ncol=K)
    ## XT_i <- array(NA, c(nb_indiv, p, K))
    ## U <- matrix(NA, nrow = nb_indiv, ncol = p)
    ## for (i in 1:nb_indiv){ #for all the genes at once
    ##     select <- indiv==levels(indiv)[i]
    ##     long_select <- long_indiv==levels(indiv)[i]
    ##     y_mu_i <- as.vector(y_mu[long_select,])
    ##     y_tilde_i <- c(y_ij)
    ##     x_tilde_i <- x_tilde[long_select,]
    ##     sigma_eps_inv_diag <- c(t(w)[select,])
    ##     T_i <- sigma_eps_inv_diag*(Phi[long_select,] %*% sig_xi_sqrt)
    ##     q[i,] <- c(y_mu_i %*% T_i)
    ##     XT_i[i,,] <- t(x_tilde_i) %*% T_i
    ##     U[i,] <- xtx_inv %*% t(x_tilde_i) %*% y_mu_i
    ## }
    ## XT <- colMeans(XT_i) q_ext <- q - U %*% XT

    sig_eps_inv_T <- t(w)
    if (length(levels(indiv)) > 1) {
        indiv_mat <- stats::model.matrix(~0 + factor(indiv))
    } else {
        indiv_mat <- matrix(as.numeric(indiv), ncol = 1)
    }
    avg_xtx_inv_tx <- nb_indiv * tcrossprod(solve(crossprod(x, x)), x)

    compute_genewise_scores <- function(v, indiv_mat_rs, avg_xtx_inv_tx, yt_mu_reshaped) {
        phi_perm <- phi[v, , drop = FALSE]
        phi_sig_xi_sqrt <- phi_perm %*% sig_xi_sqrt
        T_fast <- compute_T_cpp(sig_eps_inv_T, phi_sig_xi_sqrt)
        q_fast <- yt_mu_reshaped * T_fast
        if (na_rm && anyNA(q_fast)) {
            q_fast[is.na(q_fast)] <- 0
        }
        qq <- (indiv_mat_rs %*% q_fast)^2/nb_indiv
        return(rowSums(matrix(qq, ncol = K)))  # genewise scores
    }

    o <- order(as.numeric(unlist(split(x = as.character(seq_len(n)),
                                       f = indiv))))
    perm_list <- perm_list_cpp(as.numeric(indiv), nb_indiv, n, n_perm, o)

    if(!parallel_comp){
        if(progressbar){
            gene_Q <- pbapply::pbsapply(perm_list, compute_genewise_scores,
                                        indiv_mat_rs = rowSums(indiv_mat),
                                        avg_xtx_inv_tx = avg_xtx_inv_tx,
                                        yt_mu_reshaped = matrix(yt_mu, ncol = g * n_t, nrow = n))
        }else{
            gene_Q <- vapply(perm_list, compute_genewise_scores,
                             FUN.VALUE = rep(1.1, g),
                             indiv_mat_rs = rowSums(indiv_mat),
                             avg_xtx_inv_tx = avg_xtx_inv_tx,
                             yt_mu_reshaped = matrix(yt_mu, ncol = g * n_t, nrow = n))
        }
    }else{
        if(progressbar){
            gene_Q <- pbapply::pbsapply(perm_list, compute_genewise_scores,
                                        indiv_mat_rs = rowSums(indiv_mat),
                                        avg_xtx_inv_tx = avg_xtx_inv_tx,
                                        yt_mu_reshaped = matrix(yt_mu, ncol = g * n_t, nrow = n),
                                        cl = nb_cores)
        }else{
            gene_Q <- simplify2array(
                parallel::mclapply(X = perm_list,
                                   FUN = compute_genewise_scores,
                                   indiv_mat_rs = rowSums(indiv_mat),
                                   avg_xtx_inv_tx = avg_xtx_inv_tx,
                                   yt_mu_reshaped = matrix(yt_mu, ncol = g * n_t, nrow = n),
                                   mc.cores = nb_cores))
        }
    }
    QQ <- colSums(gene_Q)

    return(list(score = QQ[1], scores_perm = QQ[-1],
                gene_scores_unscaled = gene_Q[, 1],
                gene_scores_unscaled_perm = gene_Q[, -1]))
}
