# choose problem solvers -------------
algo_list <- list(
  algo_design = data.table::CJ(
    method = c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", 
               "RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")
  )
)


# set up problem solvers -------------
problem_solver <- function(data, method, instance, alpha = 0.05, ...) {
  Rt <- instance$Rt
  incidence <- instance$incidence
  Rt_case <- instance$Rt_case
  len <- length(Rt)
  gamma_pars <- instance$gamma_pars
  
  # for SI misspecification
  w <- instance$total_infect
  
  # define functions
  generate_SI <- function(method = c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS"),
                          shape, scale, len = 300) {
    Dmax <- floor(stats::qgamma(0.9999, shape, scale = scale))
    prob_gamma <- discretize_gamma(1:Dmax, shape = shape, scale = scale)
    if (method == "EpiLPS") {
      return(prob_gamma)
    } else if (method == "EpiEstim(week)") {
      t_start <- seq(2, len - 6)
      t_end <- t_start + 6
      config <- make_config(list(
        si_distr = c(0, prob_gamma),
        t_start = t_start,
        t_end = t_end
      ))
      return(config)
    } else if (method == "EpiEstim(month)") {
      t_start <- seq(2, len - 29)
      t_end <- t_start + 29
      config <- make_config(list(
        si_distr = c(0, prob_gamma),
        t_start = t_start,
        t_end = t_end
      ))
      return(config)
    }
  }
  fill_NAs <- function(Rt_fitted, n) {
    non_nas <- which(!is.na(Rt_fitted))
    m <- length(non_nas)
    return(c(rep(-1, n-m), Rt_fitted[non_nas]))
  }
  
  epiFilter <- function(Rgrid, m, eta, pR0, nday, Lday, Iday, a){
    
    # Probability vector for R and prior
    pR = matrix(0, nday, m); pRup = pR
    pR[1, ] = pR0; pRup[1, ] = pR0
    
    # Mean and median estimates
    Rmean = rep(0, nday); Rmed = Rmean
    # 50% and 95% (depends on a) confidence on R
    Rhat = matrix(0, 4, nday)
    
    # Initialise mean
    Rmean[1] = pR[1, ]%*%Rgrid
    # CDF of prior 
    Rcdf0 = cumsum(pR0)
    # Initialise quartiles
    idm = which(Rcdf0 >= 0.5, 1); Rmed[1] = Rgrid[idm[1]]
    id1 = which(Rcdf0 >= a, 1); id2 = which(Rcdf0 >= 1-a, 1)
    id3 = which(Rcdf0 >= 0.25, 1); id4 = which(Rcdf0 >= 0.75, 1)
    Rhat[1, 1] = Rgrid[id1[1]]; Rhat[2, 1] = Rgrid[id2[1]]
    Rhat[3, 1] = Rgrid[id3[1]]; Rhat[4, 1] = Rgrid[id4[1]]
    
    # Precompute state distributions for R transitions
    pstate = matrix(0, m, m);
    for(j in 1:m){
      pstate[j, ] = dnorm(Rgrid[j], Rgrid, sqrt(Rgrid)*eta)
    }
    
    # Update prior to posterior sequentially
    for(i in 2:nday){
      # Compute mean from Poisson renewal (observation model)
      rate = Lday[i]*Rgrid
      # Probabilities of observations
      pI = dpois(Iday[i], rate)
      
      # State predictions for R
      pRup[i, ]  = pR[i-1, ]%*%pstate
      # Update to posterior over R
      pR[i, ] = pRup[i, ]*pI
      pR[i, ] = pR[i, ]/sum(pR[i, ])
      
      # Posterior mean and CDF
      Rmean[i] = pR[i, ]%*%Rgrid
      Rcdf = cumsum(pR[i, ])
      
      # Quantiles for estimates
      idm = which(Rcdf >= 0.5, 1); Rmed[i] = Rgrid[idm[1]]
      id1 = which(Rcdf >= a, 1); id2 = which(Rcdf >= 1-a, 1)
      id3 = which(Rcdf >= 0.25, 1); id4 = which(Rcdf >= 0.75, 1)
      Rhat[1, i] = Rgrid[id1[1]]; Rhat[2, i] = Rgrid[id2[1]]
      Rhat[3, i] = Rgrid[id3[1]]; Rhat[4, i] = Rgrid[id4[1]]
    }
    
    # Main outputs: estimates of R and states
    epiFilter = list(Rmed, Rhat, Rmean, pR, pRup, pstate)
  }
  epiSmoother <- function(Rgrid, m, pR, pRup, nday, pstate, a){
    
    # Last smoothed distribution same as filtered
    qR = matrix(0, nday, m); qR[nday, ] = pR[nday, ]
    
    # Main smoothing equation iteratively computed
    for(i in seq(nday-1, 1)){
      # Remove zeros
      pRup[i+1, pRup[i+1, ] == 0] = 10^-8
      
      # Integral term in smoother
      integ = qR[i+1, ]/pRup[i+1, ]
      integ = integ%*%pstate
      
      # Smoothed posterior over Rgrid
      qR[i, ] = pR[i, ]*integ
      # Force a normalisation
      qR[i, ] = qR[i, ]/sum(qR[i, ]);
    }
    
    # Mean, median estimats of R
    Rmean = rep(0, nday); Rmed = Rmean
    # 50% and 95% (depends on a) confidence on R
    Rhat = matrix(0, 4, nday)
    
    # Compute at every time point
    for (i in 1:nday) {
      # Posterior mean and CDF
      Rmean[i] = qR[i, ]%*%Rgrid
      Rcdf = cumsum(qR[i, ])
      
      # Quantiles for estimates
      idm = which(Rcdf >= 0.5); Rmed[i] = Rgrid[idm[1]]
      id1 = which(Rcdf >= a, 1); id2 = which(Rcdf >= 1-a, 1)
      id3 = which(Rcdf >= 0.25, 1); id4 = which(Rcdf >= 0.75, 1)
      Rhat[1, i] = Rgrid[id1[1]]; Rhat[2, i] = Rgrid[id2[1]]
      Rhat[3, i] = Rgrid[id3[1]]; Rhat[4, i] = Rgrid[id4[1]]
    }
    
    # Main outputs: estimates of R and states
    epiSmoother = list(Rmed, Rhat, Rmean, qR)
  }
  
  # Configuration: use "true" serial interval distribution
  if (method %in% c("EpiEstim(week)", "EpiEstim(month)")) {
    config <- generate_SI(method, gamma_pars[1], gamma_pars[2], len = len)
  } else if (method == "EpiLPS") {
    si <- generate_SI(method, gamma_pars[1], gamma_pars[2], len = len)
  } else if (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")) {
    korder <- as.double(gsub("\\D", "", method))
    method <- "RtEstim"
    if (len > 50) nfold <- 10
    else nfold <- 5
  }
  
  # Estimate Rt and save running times
  if (method %in% c("EpiEstim(week)", "EpiEstim(month)")) {
    fit <- EpiEstim::estimate_R(
      incid = incidence,
      config = config,
      method = "non_parametric_si"
    )
    l <- which(colnames(fit$R) == paste0("Quantile.",alpha/2,"(R)"))
    u <- which(colnames(fit$R) == paste0("Quantile.",1 - alpha/2,"(R)"))
    lower_ci <- fill_NAs(fit$R[,l], len) 
    upper_ci <- fill_NAs(fit$R[,u], len)
    Rt_fitted <- fill_NAs(fit$R$`Mean(R)`, len)
  } else if (method == "EpiLPS") {
    fit <- EpiLPS::estimR(
      incidence = incidence, si = si, K = 40, 
      CoriR = FALSE
    )
    l <- which(colnames(fit$RLPS) == paste0("Rq", alpha/2)) # alpha can only by 0.05, 0.1, 0.5
    u <- which(colnames(fit$RLPS) == paste0("Rq", 1 - alpha/2))
    
    Rt_fitted <- fill_NAs(fit$RLPS$R, len)
    lower_ci <- fill_NAs(fit$RLPS[,l], len)
    upper_ci <- fill_NAs(fit$RLPS[,u], len)
  } else if (method == "EpiFilter") {
    m <- 2000
    pR0 = (1 / m) * rep(1, m)
    Rgrid = seq(0.01, 10, length.out = m)
    R_filter <- epiFilter(Rgrid, m, eta=0.1, pR0, len, w, incidence, a = alpha)
    R_smoother <-
      epiSmoother(Rgrid, m, R_filter[[4]], R_filter[[5]], len, R_filter[[6]], a =
                    alpha)
    Rt_fitted <- R_smoother[[3]]
    lower_ci <- R_smoother[[2]][1,]
    upper_ci <- R_smoother[[2]][2,]
  } else if (method == "RtEstim") {
    cv_mod <- rtestim::cv_estimate_rt(
      observed_counts = incidence,
      korder = korder, nfold = nfold,
      nsol = 50, maxiter = 3e7L, 
      dist_gamma = gamma_pars,
      error_measure = "deviance",
      lambda_min_ratio = 1e-6,
      x = 1:len, delay_distn = NULL,
      delay_distn_periodicity = NULL
    )
    ci <- confband(cv_mod, "lambda.min", 1-alpha)
    Rt_fitted <- ci[,1][[1]]
    lower_ci <- ci[,2][[1]]
    upper_ci <- ci[,3][[1]]
  }
  
  count_coverage <- function(Rt, lower, upper) {
    count <- sum(Rt >= lower & Rt <= upper)
    return(count)
  }
  m <- length(lower_ci)
  Rt <- Rt[(len - m + 1) : len] # truncate Rt
  ci_coverage <- mapply(count_coverage, Rt, lower_ci, upper_ci)
  
  compute_interval_score <- function(Rt, l, u, alpha) {
    ci_score <- (u - l) + 2 / alpha * (l - Rt) * (Rt < l) + 2 / alpha * (Rt - u) * (Rt > u) 
    ci_score <- mean(ci_score)
    return(ci_score)
  }
  
  lst <- list()
  lst[["fitted_Rt"]] <- Rt_fitted
  lst[["lower_bound"]] <- lower_ci
  lst[["upper_bound"]] <- upper_ci
  lst[["ci_coverage"]] <- ci_coverage
  lst[["ci_percentage"]] <- mean(ci_coverage)
  lst[["ci_score"]] <- compute_interval_score(Rt, lower_ci, upper_ci, alpha)
  return(lst)
}

