source("src/experiments/load_functions.R")

# choose problem solvers -------------
algo_designs <- list(
  rt_solver = data.table::CJ(
    method = c('EpiEstim', 'RtEstim', 'EpiLPS')
  )
)

# set up problem solvers -------------
problem_solver <- function(data, method, instance, ...){
  Rt <- instance$Rt
  incidence <- instance$incidence
  Rt_case <- instance$Rt_case
  
  # Configuration: use "true" serial interval distribution 
  if(method == "EpiEstim"){
    if (Rt_case == "1"){
      prob_gamma <- c(0, diff(c(0, pgamma(1:11, 3, scale=3))))
      prob_gamma <- prob_gamma / sum(prob_gamma)
    } else {
      prob_gamma <- c(0, diff(c(0, pgamma(1:11, 3.5, scale=3.5))))
      prob_gamma <- prob_gamma / sum(prob_gamma)
    }
    config <- make_config(list(si_distr = prob_gamma)) 
  } else if (method == "RtEstim") {
    korder <- switch(Rt_case, "1" = 0, "2" = 3, "3" = 1, "4" = 3)
  } else if (method == "EpiLPS") {
    if(Rt_case == "1") {
      si <- EpiLPS::Idist(mean = 9, sd = 27, dist="gamma")$pvec[1:30]
      si <- si / sum(si)
    } else {
      si <- EpiLPS::Idist(mean = 3.5^2, sd = 3.5^3, dist="gamma")$pvec[1:30]
      si <- si / sum(si)
    }
    
  }
  
  # Estimate Rt and save running times
  runtime <- switch(
    method,
    "EpiEstim" = microbenchmark(
      {
        Rt_fitted <- EpiEstim::estimate_R(
          incid = incidence,
          config = config,
          method = "non_parametric_si")$R$`Mean(R)`
        # fill Rt for the first week by?
        Rt_fitted <- c(rep(Rt_fitted[1], 7), Rt_fitted)
      },
      times = 10, unit = "us"
    ),
    "RtEstim" = microbenchmark(
      {
        cv_mod <- rtestim::cv_estimate_rt(incidence, korder=korder, nfold=3, 
                                          nsol=50, maxiter = 1e7L, 
                                          dist_gamma = c(3, 3))
        Rt_fitted <- cv_mod$full_fit$Rt[ ,which.min(cv_mod$cv_scores)]
      },
      times = 10, unit = "us"
    ),
    "EpiLPS" = microbenchmark(
      {
        Rt_fitted <- EpiLPS::estimR(incidence = incidence, si = si, 
                                    CoriR = TRUE)$RLPS$R
      },
      times = 10, unit = "us"
    )
  )
  
  # Save running times and Rt accuracy
  mean_runtime <- as.list(summary(runtime))$mean[1]
  # compute mean KL / Rt ratio
  Rt_kl_long <- mean(Rt_fitted * log(Rt_fitted / Rt) + Rt - Rt_fitted)
  
  Rt_kl <- mean(Rt_fitted[8:300] * log(Rt_fitted[8:300]/Rt[8:300]) - 
                     Rt_fitted[8:300] + Rt[8:300]) # remove the first week
  Rt_ratio_long <- mean(log(Rt_fitted / Rt))
  Rt_ratio <- mean(log(Rt_fitted[8:300] / Rt[8:300]))
  
  lst <- list()
  lst[["runtime"]] <- mean_runtime
  lst[["Rt_kl_long"]] <- Rt_kl_long
  lst[["Rt_kl"]] <- Rt_kl
  lst[["Rt_ratio_long"]] <- Rt_ratio_long
  lst[["Rt_ratio"]] <- Rt_ratio
  return(lst)
}
