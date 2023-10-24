# choose problem solvers -------------
algo_designs <- list(
  epiestim_week = data.table(
    method = 'EpiEstim(week)'
  ),
  epiestim_month = data.table(
    method = 'EpiEstim(month)'
  ),
  rtestim = data.table(
    method = 'RtEstim'
  ),
  epilps = data.table(
    method = 'EpiLPS'
  )
)

# set up problem solvers -------------
problem_solver <- function(data, method, instance, ...){
  Rt <- instance$Rt
  incidence <- instance$incidence
  Rt_case <- instance$Rt_case
  len <- length(Rt)
  gamma_pars <- instance$gamma_pars
  
  # Configuration: use "true" serial interval distribution 
  if(method %in% c("EpiEstim(week)", "EpiEstim(month)")){
    config <- generate_SI(method, gamma_pars[1], gamma_pars[2])
  } else if (method == "RtEstim") {
    korder <- switch(Rt_case, "1" = 0, "2" = c(1,3), "3" = 1, "4" = 3)
  } else if (method == "EpiLPS") {
    si <- generate_SI(method, gamma_pars[1], gamma_pars[2])
  }
  
  # Estimate Rt and save running times
  runtime <- switch(
    method,
    "EpiEstim(week)" = microbenchmark(
      {
        Rt_fitted <- EpiEstim::estimate_R(
          incid = incidence,
          config = config,
          method = "non_parametric_si")$R$`Mean(R)`
        Rt_fitted <- impute_NAs(Rt_fitted)
        Rt_fitted <- c(rep(NA, 7), Rt_fitted)
      },
      times = 10, unit = "us"
    ),
    "EpiEstim(month)" = microbenchmark(
      {
        Rt_fitted <- EpiEstim::estimate_R(
          incid = incidence,
          config = config,
          method = "non_parametric_si")$R$`Mean(R)`
        Rt_fitted <- impute_NAs(Rt_fitted) # impute the first few NAs in the fitted Rt
        Rt_fitted <- c(rep(NA, 30), Rt_fitted) # fill the first month by NAs
      },
      times = 10, unit = "us"
    ),
    "RtEstim" = microbenchmark(
      {
        if(length(korder) == 1){
          cv_mod <- rtestim::cv_estimate_rt(incidence, korder=korder, nfold=3, 
                                            nsol=50, maxiter = 1e7L, 
                                            dist_gamma = c(3, 3),
                                            error_measure = "mse")
          Rt_fitted <- cv_mod$full_fit$Rt[ ,which.min(cv_mod$cv_scores)]
        } else {
          Rt_fitted <- list()
          for(i in 1:length(korder)) {
            cv_mod <- rtestim::cv_estimate_rt(incidence, korder=korder[i], nfold=3, 
                                              nsol=50, maxiter = 1e7L, 
                                              dist_gamma = c(3, 3),
                                              error_measure = "mse")
            Rt_fitted[[i]] <- cv_mod$full_fit$Rt[ ,which.min(cv_mod$cv_scores)]
          }
        }
      },
      times = 10, unit = "us"
    ),
    "EpiLPS" = microbenchmark(
      {
        Rt_fitted <- EpiLPS::estimR(incidence = incidence, si = si, 
                                    CoriR = TRUE)$RLPS$R
        Rt_fitted[1:7] <- NA # drop the first week in visualization&accuracy measurement
      },
      times = 10, unit = "us"
    )
  )
  
  # Save running times and Rt accuracy
  mean_runtime <- as.list(summary(runtime))$mean[1]
  # compute mean KL / Rt ratio
  if(method == "EpiEstim(month)"){
    Rt_kl_pois <- NA
    Rt_kl_pois_month <- compute_kl_pois(Rt_fitted[31:len], Rt[31:len])
  } else if (method == "EpiEstim(week)") {
      Rt_kl_pois <- compute_kl_pois(Rt_fitted[8:len], Rt[8:len])
      Rt_kl_pois_month <- NA
  } else if (Rt_case == 2 && method=="RtEstim") {
      Rt_kl_pois <- list()
      Rt_kl_pois_month <- list()
      for(i in 1:length(korder)){
        Rt_kl_pois[[i]] <- compute_kl_pois(Rt_fitted[[i]][8:len], Rt[8:len])
        Rt_kl_pois_month[[i]] <- compute_kl_pois(Rt_fitted[[i]][31:len], Rt[31:len])
      }
  } else {
      Rt_kl_pois <- compute_kl_pois(Rt_fitted[8:len], Rt[8:len])
      Rt_kl_pois_month <- compute_kl_pois(Rt_fitted[31:len], Rt[31:len])
  }
  #Rt_kl_base <- compute_kl_base(incidence[8:len], Rt[8:len]) # NAs
  
  lst <- list()
  lst[["runtime"]] <- mean_runtime
  lst[["Rt_kl"]] <- Rt_kl_pois
  lst[["Rt_kl_month"]] <- Rt_kl_pois_month
  return(lst)
}

compute_kl_base <- function(y, Rt){
  w <- rtestim::delay_calculator(y)
  kl <- compute_kl_pois(y/w, Rt) # -Infs in kl, 0s in y
  return(kl)
}

compute_kl_pois <- function(Rt_fitted, Rt){
  kl <- Rt_fitted * log(Rt_fitted/Rt) - Rt_fitted + Rt
  return(mean(kl))
}

compute_kl_nb <- function(mu1, mu2, size) {
  kl_div <- mu1 * log(mu1/mu2) + (mu1+size) * log((mu2+size)/(mu1+size))
  return( mean(kl_div) ) 
}

generate_SI_EpiLPS <- function(shape, scale) {
  Dmax <- floor(stats::qgamma(0.9999, shape, scale = scale))
  si <- EpiLPS::Idist(mean = shape*scale, sd = sqrt(shape*scale^2), 
                      dist="gamma")$pvec[1:Dmax]
  si <- si / sum(si)
  return(si)
}

generate_SI <- function(method = c('EpiEstim(week)', 'EpiEstim(month)', 'EpiLPS'), 
                                 shape, scale, len=300){
  Dmax <- floor(stats::qgamma(0.9999, shape, scale = scale))
  prob_gamma <- discretize_gamma(1:Dmax, shape = shape, scale = scale)
  if(method == "EpiLPS") {
    return(prob_gamma)
  } else if(method == "EpiEstim(week)") {
    t_start <- seq(2, len - 6) 
    t_end <- t_start + 6
    config <- make_config(list(si_distr = c(0, prob_gamma),
                               t_start = t_start,
                               t_end = t_end)) 
    return(config)
  } else if (method == "EpiEstim(month)") {
    t_start <- seq(2, len - 29) 
    t_end <- t_start + 29
    config <- make_config(list(si_distr = c(0, prob_gamma),
                               t_start = t_start,
                               t_end = t_end)) 
    return(config)
  }
}

impute_NAs <- function(Rt_fitted) {
    non_nas <- which(!is.na(Rt_fitted))
    if (length(non_nas) == length(Rt_fitted)) {
      return(Rt_fitted)
    } else {
      Rt_fitted[1:(non_nas[1]-1)] <- Rt_fitted[non_nas[1]]
      stopifnot(sum(is.na(Rt_fitted)) == 0)
      return(Rt_fitted)
    } 
}
