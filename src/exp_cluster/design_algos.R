# choose problem solvers -------------
algo_list <- list(
  algo_design = data.table::CJ(
    method = c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter",
               "RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)"),
    si_pars = c("measles", "SARS") 
  )
)
algo_list2 <- list(
  algo_design_all = data.table::CJ(
    method = c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2", 
               "RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)"),
    si_pars = c("flu", "measles")
  )
)
algo_list_mis_si <- list(
  algo_design_si = data.table::CJ(
    method = c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter",
               "RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)"),
    si_pars = "measles_ss" # shifted and scaled measles par's
  )
)
algo_list_mis_si_short <- list(
  algo_design_si_short = data.table::CJ(
    method = c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2", 
               "RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)"),
    si_pars = "flu_ss" # shifted and scaled flu par's
  )
)
algo_list_epinow2 <- list(
  algo_design_epinow2 = data.table::CJ(
    method = "EpiNow2",
    si_pars = "measles" 
  )
)

# set up problem solvers -------------
problem_solver <- function(data, method, instance, si_pars = "measles", ...) {
  Rt <- instance$Rt
  incidence <- instance$incidence
  Rt_case <- instance$Rt_case
  len <- length(Rt)
  #gamma_pars <- instance$gamma_pars
  if (!is.null(si_pars)) {
    gamma_pars <- switch(si_pars,
                         "flu" = c(3.0044, 0.8654),
                         "SARS" = c(4.8864, 1.7190),
                         "measles" = c(14.5963, 1.0208),
                         "measles_ss" = c(15.51882, 1.08900), 
                         "flu_ss" = c(7.7722681, 0.5918478), 
                         c(14.5963, 1.0208))
  }
  
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
  impute_NAs <- function(Rt_fitted) {
    non_nas <- which(!is.na(Rt_fitted))
    if (length(non_nas) == length(Rt_fitted)) {
      return(Rt_fitted)
    } else {
      Rt_fitted[1:(non_nas[1] - 1)] <- Rt_fitted[non_nas[1]]
      stopifnot(sum(is.na(Rt_fitted)) == 0)
      return(Rt_fitted)
    }
  }
  compute_kl_pois <- function(Rt_true, Rt_fitted, w) {
    logRtfit <- Rt_fitted
    logRtfit[logRtfit == 0] <- 1
    logRtfit <- log(logRtfit)
    kl <- Rt_true * log(Rt_true) -  Rt_true * logRtfit - Rt_true + Rt_fitted
    return(mean(w * kl))
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
  runtime <- switch(method,
    "EpiEstim(week)" = microbenchmark(
      {
        Rt_fitted <- EpiEstim::estimate_R(
          incid = incidence,
          config = config,
          method = "non_parametric_si"
        )$R$`Mean(R)`
        Rt_fitted <- impute_NAs(Rt_fitted)
        Rt_fitted <- c(rep(NA, 7), Rt_fitted)
      },
      times = 10,
      unit = "us"
    ),
    "EpiEstim(month)" = microbenchmark(
      {
        Rt_fitted <- EpiEstim::estimate_R(
          incid = incidence,
          config = config,
          method = "non_parametric_si"
        )$R$`Mean(R)`
        Rt_fitted <- impute_NAs(Rt_fitted) 
        Rt_fitted <- c(rep(NA, 30), Rt_fitted) 
      },
      times = 10,
      unit = "us"
    ),
    "EpiLPS" = microbenchmark(
      {
        Rt_fitted <- EpiLPS::estimR(
          incidence = incidence, si = si, K = 40,
          CoriR = FALSE
        )$RLPS$R
        Rt_fitted[1:7] <- NA 
      },
      times = 10,
      unit = "us"
    ),
    "EpiFilter" = microbenchmark(
      {
        m <- 2000
        pR0 = (1 / m) * rep(1, m)
        Rgrid = seq(0.01, 10, length.out = m)
        total_infect <- delay_calculator(incidence, dist_gamma = gamma_pars)
        R_filter <- epiFilter(Rgrid, m, eta=0.1, pR0, len, total_infect, incidence, a=0.25)
        R_smoother <- epiSmoother(Rgrid, m, R_filter[[4]], R_filter[[5]], len, R_filter[[6]], a=0.25)
        Rt_fitted <- R_smoother[[3]]
      },
      times = 10,
      unit = "us"
    ),
    "EpiNow2" = microbenchmark(
      {
        dates <- seq(from = as.Date("2023-01-01"), by = "day", length.out = len)
        out <- epinow(reported_cases = data.table(confirm=incidence, date=dates),
                      generation_time = generation_time_opts(
                        dist_spec(mean = gamma_pars[1]*gamma_pars[2], 
                                  sd = sqrt(gamma_pars[1]*gamma_pars[2]*gamma_pars[2]),
                                  max = 50)
                      ),
                      rt = rt_opts(),
                      delays = delay_opts()
        )
        Rt_fitted <- out$estimates$summarised$mean[1:len]
      },
      times = 10,
      unit = "us"
    ),
    "RtEstim" = microbenchmark(
      {
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
        Rt_fitted <- cv_mod$full_fit$Rt[, which.min(cv_mod$cv_scores)]
      },
      times = 10,
      unit = "us"
    )
  )
  
  # Save running times and Rt accuracy
  mean_runtime <- as.list(summary(runtime))$mean[1]
  
  # compute mean KL
  if (method == "EpiEstim(month)") {
    Rt_kl_pois <- NA
    Rt_kl_pois_month <- compute_kl_pois(Rt[31:len], Rt_fitted[31:len], w[31:len])
  } else if (method == "EpiEstim(week)") {
    Rt_kl_pois <- compute_kl_pois(Rt[8:len], Rt_fitted[8:len], w[8:len])
    Rt_kl_pois_month <- NA
  } else {
    Rt_kl_pois <- compute_kl_pois(Rt[8:len], Rt_fitted[8:len], w[8:len])
    Rt_kl_pois_month <- compute_kl_pois(Rt[31:len], Rt_fitted[31:len], w[31:len])
  }
  
  lst <- list()
  lst[["runtime"]] <- mean_runtime
  lst[["Rt_kl"]] <- Rt_kl_pois
  lst[["Rt_kl_month"]] <- Rt_kl_pois_month
  return(lst)
}

