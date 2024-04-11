# choose problem solvers -------------
algo_list <- list(
  algo_design = data.table::CJ(
    method = c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2", 
               "RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)"),
    si_pars = c("measles", "SARS")
  )
)


# set up problem solvers -------------
problem_solver <- function(data, method, instance, si_pars = NULL, ...) {
  Rt <- instance$Rt
  incidence <- instance$incidence
  Rt_case <- instance$Rt_case
  len <- length(Rt)
  gamma_pars <- instance$gamma_pars
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
    "EpiEstim(week)" = 
      {
        Rt_fitted <- EpiEstim::estimate_R(
          incid = incidence,
          config = config,
          method = "non_parametric_si"
        )$R$`Mean(R)`
        Rt_fitted <- impute_NAs(Rt_fitted)
        Rt_fitted <- c(rep(NA, 7), Rt_fitted)
      },
    "EpiEstim(month)" = 
      {
        Rt_fitted <- EpiEstim::estimate_R(
          incid = incidence,
          config = config,
          method = "non_parametric_si"
        )$R$`Mean(R)`
        Rt_fitted <- impute_NAs(Rt_fitted) 
        Rt_fitted <- c(rep(NA, 30), Rt_fitted) 
      },
    "EpiLPS" = 
      {
        Rt_fitted <- EpiLPS::estimR(
          incidence = incidence, si = si, K = 40,
          CoriR = FALSE
        )$RLPS$R
        Rt_fitted[1:7] <- NA 
      },
    "EpiFilter" = 
      {
        m <- 2000
        pR0 = (1 / m) * rep(1, m)
        Rgrid = seq(0.01, 10, length.out = m)
        R_filter <- epiFilter(Rgrid, m, eta=0.1, pR0, len, w, incidence, a=0.25)
        R_smoother <- epiSmoother(Rgrid, m, R_filter[[4]], R_filter[[5]], len, R_filter[[6]], a=0.25)
        Rt_fitted <- R_smoother[[3]]
      },
    "EpiNow2" = 
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
    "RtEstim" = 
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
      }
  )
  
  
  lst <- list()
  lst[["Rt_fitted"]] <- Rt_fitted
  return(lst)
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

gamma_params <- function(mean_val, sd_val) {
  shape <- (mean_val / sd_val)^2
  scale <- (sd_val^2) / mean_val
  return(c(shape, scale))
}
