# choose problem solvers -------------
algo_design1 <- list(
  epiestim_week = data.table(
    method = "EpiEstim(week)"
  ),
  epiestim_month = data.table(
    method = "EpiEstim(month)"
  ),
  epilps = data.table(
    method = "EpiLPS"
  )
)
algo_design2 <- list(
  rtestim0 = data.table(
    method = "RtEstim(k=0)"
  )
)
algo_design3 <- list(
  rtestim1 = data.table(
    method = "RtEstim(k=1)"
  )
)
algo_design4 <- list(
  rtestim3 = data.table(
    method = "RtEstim(k=3)"
  )
)

# set up problem solvers -------------
problem_solver <- function(data, method, instance, ...) {
  Rt <- instance$Rt
  incidence <- instance$incidence
  Rt_case <- instance$Rt_case
  len <- length(Rt)
  gamma_pars <- instance$gamma_pars
  w <- instance$total_infect
  w <- w / sum(w)

  # Configuration: use "true" serial interval distribution
  if (method %in% c("EpiEstim(week)", "EpiEstim(month)")) {
    config <- generate_SI(method, gamma_pars[1], gamma_pars[2])
  } else if (method == "EpiLPS") {
    si <- generate_SI(method, gamma_pars[1], gamma_pars[2])
  } else if (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=3)")) {
    korder <- as.double(gsub("\\D", "", method))
    method <- "RtEstim"
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
          incidence = incidence, si = si,
          CoriR = TRUE
        )$RLPS$R
        Rt_fitted[1:7] <- NA 
      },
      times = 10,
      unit = "us"
    ),
    "RtEstim" = microbenchmark(
      {
        cv_mod <- rtestim::cv_estimate_rt(
          incidence,
          korder = korder, nfold = 10,
          nsol = 50, maxiter = 3e7L, 
          dist_gamma = gamma_pars,
          error_measure = "deviance",
          lambda_min_ratio = 1e-6
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

compute_kl_pois <- function(Rt_true, Rt_fitted, w) {
  logRtfit <- Rt_fitted
  logRtfit[logRtfit == 0] <- 1
  logRtfit <- log(logRtfit)
  kl <- Rt_true * log(Rt_true) -  Rt_true * logRtfit - Rt_true + Rt_fitted
  return(mean(w * kl))
}

generate_SI_EpiLPS <- function(shape, scale) {
  Dmax <- floor(stats::qgamma(0.9999, shape, scale = scale))
  si <- EpiLPS::Idist(
    mean = shape * scale, sd = sqrt(shape * scale^2),
    dist = "gamma"
  )$pvec[1:Dmax]
  si <- si / sum(si)
  return(si)
}

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
