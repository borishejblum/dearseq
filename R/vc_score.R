#'Computes variance component score test statistics
#'
#'This function computes the variance component score test statistics
#'
#'@keywords internal
#'
#'@param y a numeric matrix of dim \code{g x n} containing the raw RNA-seq
#'counts for g genes from \code{n} samples.
#'
#'@param x a numeric design matrix of dim \code{n x p} containing the \code{p}
#'covariates to be adjusted for.
#'
#'@param indiv a vector of length \code{n} containing the information for
#'attributing each sample to one of the studied individuals. Coerced
#'to be a \code{factor}.
#'
#'@param phi a numeric design matrix of size \code{n x K} containing the
#'\code{K} variables to .be tested
#'
#'@param w a vector of length \code{n} containing the weights for the \code{n}
#'samples.
#'
#'@param Sigma_xi a matrix of size \code{K x K} containing the covariance matrix
#'of the \code{K} random effects on \code{phi}.
#'
#'@param na_rm logical: should missing values (including \code{NA} and
#'\code{NaN}) be omitted from the calculations? Default is \code{FALSE}.
#'
#'@return A list with the following elements:\itemize{
#'   \item \code{score}: approximation of the set observed score
#'   \item \code{q}: observation-level contributions to the score
#'   \item \code{q_ext}: pseudo-observations used to compute the covariance,
#'    taking into account the contributions of OLS estimates
#'   \item \code{gene_scores_unscaled}: a vector of the approximations of the
#'   individual gene scores
#' }
#'
#'@examples
#'set.seed(123)
#'
#'##generate some fake data
#'########################
#'n <- 100
#'r <- 12
#'t <- matrix(rep(1:3), r/3, ncol=1, nrow=r)
#'sigma <- 0.4
#'b0 <- 1
#'
#'#under the null:
#'b1 <- 0
#'#under the alternative:
#'b1 <- 0.7
#'y.tilde <- b0 + b1*t + rnorm(r, sd = sigma)
#'y <- t(matrix(rnorm(n*r, sd = sqrt(sigma*abs(y.tilde))), ncol=n, nrow=r) +
#'       matrix(rep(y.tilde, n), ncol=n, nrow=r))
#'x <- matrix(1, ncol=1, nrow=r)
#'
#'#run test
#'scoreTest <- vc_score(y, x, phi=t, w=matrix(1, ncol=ncol(y), nrow=nrow(y)),
#'                     Sigma_xi=matrix(1), indiv=rep(1:(r/3), each=3))
#'scoreTest$score
#'
#'@importFrom stats model.matrix
#'
#'@export
vc_score <- function(y, x, indiv, phi, w, Sigma_xi = diag(ncol(phi)),
                     na_rm = FALSE) {
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
    indiv <- as.factor(indiv)
    nb_indiv <- length(levels(indiv))

    ## OLS for conditional mean -----
    y_T <- t(y)
    if (na_rm && anyNA(y_T)) {
        y_T0 <- y_T
        y_T0[is.na(y_T0)] <- 0
        yt_mu <- y_T - x %*% solve(crossprod(x)) %*% t(x) %*% y_T0
    } else {
        yt_mu <- y_T - x %*% solve(crossprod(x)) %*% t(x) %*% y_T
    }
    ## x_tilde_list <- y_tilde_list <- Phi_list <- list()
    ## for (i in 1:nb_indiv) {
    ##     select <- indiv==levels(indiv)[i]
    ##     n_i <- length(which(select))
    ##     x_i <- x[select,]
    ##     y_i <- y[,select]
    ##     phi_i <- phi[select,]
    ##     Phi_list[[i]] <- kronecker(diag(g), phi_i)
    ##     x_tilde_list[[i]] <- kronecker(diag(g), x_i)
    ##     y_tilde_list[[i]] <- matrix(t(y_i), ncol=1)
    ## }
    ## x_tilde <- do.call(rbind, x_tilde_list)
    ## y_tilde <- do.call(rbind, y_tilde_list)
    ## Phi <- do.call(rbind, Phi_list)
    ## alpha <- solve(t(x_tilde)%*%x_tilde)%*%t(x_tilde)%*%y_tilde
    ## mu_new <- x_tilde %*% alpha
    ## y_mu <- y_tilde - mu_new


    ## test statistic computation ------
    ## q <- matrix(NA, nrow=nb_indiv, ncol=g*K)
    ## XT_i <- array(NA, c(nb_indiv, g*p, g*K))
    ## U <- matrix(NA, nrow = nb_indiv, ncol = p*g)
    ## long_indiv <- rep(indiv, each = g)
    ## xtx_inv <- solve(t(x_tilde) %*% x_tilde)
    ## Sigma_xi_new_sqrt <- kronecker(diag(g), (Sigma_xi*diag(K))%^% (-0.5))
    ## for (i in 1:nb_indiv){ #for all the genes at once
    ##     select <- indiv==levels(indiv)[i]
    ##     long_select <- long_indiv==levels(indiv)[i]
    ##     y_mu_i <- as.vector(y_mu[long_select,])
    ##     y_tilde_i <- c(t(y_ij))
    ##     x_tilde_i <- x_tilde[long_select,]
    ##     sigma_eps_inv_diag <- as.vector(t(w)[select,])#/sigma
    ##     T_i <- sigma_eps_inv_diag*(Phi[long_select,] %*% Sigma_xi_new_sqrt)
    ##     q[i,] <- c(y_mu_i %*% T_i)
    ##     XT_i[i,,] <- t(x_tilde_i) %*% T_i
    ##     U[i,] <- xtx_inv %*% t(x_tilde_i) %*% y_mu_i }
    ## XT <- colMeans(XT_i) q_ext <- q - U %*% XT

    ## sig_eps_inv <- w/sigma #no need as this just scales the test statistics

    sig_xi_sqrt <- (Sigma_xi * diag(K)) %^% (-0.5)
    sig_eps_inv_T <- t(w)
    phi_sig_xi_sqrt <- phi %*% sig_xi_sqrt

    T_fast <- compute_T_cpp(sig_eps_inv_T, phi_sig_xi_sqrt)
    ##---------------------
    ## the structure of T_fast is time_basis_1*gene_1, time_basis_1*gene_2, ...,
    ## time_basis_1*gene_p, ..., time_basis_K*gene_1, ..., time_basis_K*gene_p
    ##----------------------------
    q_fast <- matrix(yt_mu, ncol = g * n_t, nrow = n) * T_fast

    ## dplyr seems to be less efficient here
    ## q_fast_tb <- tibble::as_tibble(cbind.data.frame(indiv, q_fast))
    ## q_dp <- q_fast_tb %>% group_by(indiv) %>% summarise_all(sum)

    ## aggregate is much slower also
    ## qtemp <- aggregate(. ~ indiv, cbind.data.frame(indiv, q_fast), sum)
    ## qtemp <- aggregate(. ~ indiv, cbind.data.frame(indiv, q_fast), sum)

    ## data.table hard to test, but seems to be at least 10 times slower on big
    ## datasets (weird)
    ## m_dt <- data.table('indiv'=factor(rep(c(1:20), each=5)), mbig)
    ## temp <- m_dt[, lapply(.SD, sum), by=indiv]


    # the 2 'by' statements below used to represent the longest AND most memory
    # intensive part of this for genewise analysis:
    if (length(levels(indiv)) > 1) {
        indiv_mat <- stats::model.matrix(~0 + factor(indiv))
    } else {
        indiv_mat <- matrix(as.numeric(indiv), ncol = 1)
    }

    if (na_rm && anyNA(q_fast)) {
        q_fast[is.na(q_fast)] <- 0
    }
    q <- crossprod(indiv_mat, q_fast)
    XT_fast <- crossprod(x, T_fast)/nb_indiv
    avg_xtx_inv_tx <- nb_indiv * tcrossprod(solve(crossprod(x, x)), x)
    U_XT <- matrix(yt_mu, ncol = g * n_t, nrow = n) *
        crossprod(avg_xtx_inv_tx, XT_fast)
    if (na_rm && anyNA(U_XT)) {
        U_XT[is.na(U_XT)] <- 0
    }
    U_XT_indiv <- crossprod(indiv_mat, U_XT)
    q_ext <- q - U_XT_indiv
    # sapply(1:6, function(i){(q_ext[i,] - q_ext_fast_indiv[i,])})



    qq <- colSums(q, na.rm = na_rm)^2/nb_indiv  # genewise scores

    ##qq <- unlist(by(data=matrix(qq, ncol=1), INDICES=rep(1:g, K), FUN=sum,
    ##                simplify = FALSE)) # veryslow
    ## gene_inds <- lapply(1:g, function(x){x + (g)*(0:(K-1))})
    ## gene_Q <- sapply(gene_inds, function(x) sum(qq[x])) # old computation
    ## gene_Q <- tcrossprod(qq, matrix(diag(g), nrow=g, ncol=g*K))[1, ] # faster
    gene_Q <- rowSums(matrix(qq, ncol = K))  # even faster

    QQ <- sum(qq)  #nb_indiv=nrow(q) # set score

    return(list(score = QQ, q = q, q_ext = q_ext,
                gene_scores_unscaled = gene_Q))
}
