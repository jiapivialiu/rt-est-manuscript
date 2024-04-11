## create problems -------------
prob_list <- list(
  prob_design = data.table::CJ(Rt_case = 1:4, 
                               dist = c("poisson", "NB"), 
                               len = c(50, 300), 
                               si_type = c("SARS", "measles"))
)


## generate data -----------------------
data_generator <- function(data = NULL, job, Rt_case, len = 300, dist = c("poisson", "NB"), 
                           si_type = "measles", seed = NULL, ...){
  # General settings: 
  N1 <- 2 # mean of the first incidence data
  
  get_Rt4 <- function(length) {
    Rt <- numeric(length) 
    components <- list(
      list(freq = 0.1, amp = 1),
      list(freq = 0.5, amp = 2),
      list(freq = 1.0, amp = 3)
    )
    x <- seq(0, 10, length.out = length)
    for (component in components) {
      Rt <- Rt + 0.2 * (component$amp * sin(pi * component$freq * x / 1.2) + 
                          component$amp)
    }
    return(Rt)
  }
  Rt <- switch(
    Rt_case,
    "1" = c(rep(2, floor(2 * len / 5)), rep(0.8, len - floor(2 * len / 5))),
    "2" = c(exp(.01 * (1:floor(len / 3))),
            exp(-.005 * (floor(len / 3) + 1):len) * exp(.01 * floor(len / 3))),
    "3" = c(
      seq(2.5, 2, length.out = floor(len / 4)),
      seq(0.8, 0.6, length.out = floor(len / 4)),
      seq(1.7, 2, length.out = floor(len / 4)),
      seq(0.9, 0.5, length.out = len - 3 * floor(len / 4))
    ),
    "4" = get_Rt4(len)
  )
  
  # get incidence
  Mu <- double(len) # N_1:n
  incidence <- double(len) # y_1:n
  w <- double(len)
  Mu[1] <- N1
  # use epidemic gamma parameters
  gamma_pars <- switch(si_type,
                       "flu" = c(3.0044, 0.8654),
                       "SARS" = c(4.8864, 1.7190),
                       "measles" = c(14.5963, 1.0208))
  if (!is.null(seed))
    set.seed(seed)
  if(dist == "poisson"){
    incidence[1] <- Mu[1]
    for(t in 2:len){
      if (t == 2) w[t] <- incidence[1]
      else {w[t] <- delay_calculator(incidence[1:t], dist_gamma = gamma_pars)[t]}
      Mu[t] <- Rt[t] * w[t]
      incidence[t] <- rpois(1, Mu[t])
    }
  } else if (dist == "NB") { # set overdispersion size=5
    size = 5
    incidence[1] <- Mu[1]
    for(t in 2:len){
      if(t == 2) w[t] <- incidence[1]
      else {w[t] <- delay_calculator(incidence[1:t], dist_gamma = gamma_pars)[t]}
      Mu[t] <- Rt[t] * w[t]
      incidence[t] <- rnbinom(1, mu = Mu[t], size = size)
    }
  }
  w <- c(w[2], w[-1])
  
  ## return list
  lst <- list() 
  lst[["Rt"]] <- Rt
  lst[["incidence"]] <- incidence
  lst[["Rt_case"]] <- Rt_case
  lst[["gamma_pars"]] <- gamma_pars
  lst[["total_infect"]] <- w
  lst[["mean"]] <- Mu
  return(lst) # input as `instance` in problem solver
}
