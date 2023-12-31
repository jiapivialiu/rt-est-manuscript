## create problems -------------
problem_design1 <- list(
  # 4 scenarios of Rt curvatures
  pois_scenario1 = data.table::CJ(
    Rt_case = 1,
    dist = "poisson" # Poisson-distributed incidence
  ),
  NB_scenario1 = data.table::CJ(
    Rt_case = 1,
    dist = "NB" # negative Binomial-distributed incidence
  )
)
problem_design2 <- list(
  pois_scenario2 = data.table::CJ(
    Rt_case = 2,
    dist = "poisson"
  ),
  NB_scenario2 = data.table::CJ(
    Rt_case = 2,
    dist = "NB"
  )
)
problem_design3 <- list(
  pois_scenario3 = data.table::CJ(
    Rt_case = 3,
    dist = "poisson"
  ),
  NB_scenario3 = data.table::CJ(
    Rt_case = 3,
    dist = "NB"
  )
)
problem_design4 <- list(
  pois_scenario4 = data.table::CJ(
    Rt_case = 4,
    dist = "poisson" 
  ),
  NB_scenario4 = data.table::CJ(
    Rt_case = 4,
    dist = "NB" 
  )
)

## generate data -----------------------
data_generator <- function(data=NULL, job, Rt_case, dist = c("poisson", "NB"), ...){
  # General settings: 
  N1 = 2 # first incidence data
  len = 300 # number of evenly spaced time points
  Rt <- get_rt(Rt_case, len)
  incidence <- get_incidence(N1, Rt, Rt_case, dist)
  y <- incidence$y
  w <- incidence$w
  gamma_pars <- incidence$gamma_pars
    
  ## return list
  lst <- list() 
  lst[["Rt"]] <- Rt
  lst[["incidence"]] <- y
  lst[["Rt_case"]] <- Rt_case
  lst[["gamma_pars"]] <- gamma_pars
  lst[["total_infect"]] <- w
  return(lst) # input as `instance` in problem solver
}

get_rt <- function(Rt_case, length){
  Rt <- switch(Rt_case,
               "1" = c(rep(2, 70), rep(0.8, length - 70)),
               "2" = c(exp(.015 * (1:50)), 
                     exp(-.005 * (51:length)) * exp(.015 * 50)),
               "3" = c(seq(2.5, 2, length.out = 60), 
                     seq(0.8, 0.6, length.out = 50), 
                     seq(1.7, 2, length.out = 40), 
                     seq(0.9, 0.5, length.out = 150)),
               "4" = get_rt_case4(length)
  )
  return(Rt)
}

get_rt_case4 <- function(len){
  x <- seq(0, 10, length.out = len)
  Rt4 <- numeric(len)
  components <- list(
    list(freq = 0.1, amp = 1),
    list(freq = 0.5, amp = 2),
    list(freq = 1.0, amp = 3)
  )
  for (component in components) {
    Rt4 <- Rt4 + 0.2 * (component$amp * sin(pi * component$freq * x / 1.2) + 
                          component$amp)
  }
  return(Rt4)
}

# Get Poisson incidence cases: 
get_incidence <- function(N1, Rt, Rt_case, dist){
  len <- length(Rt)
  incidence <- double(len) # N_1:n
  count <- double(len) # y_1:n
  w <- double(len)
  incidence[1] <- N1
  gamma_pars <- switch(Rt_case,
                       "1" = c(3, 3),
                       "2" = c(2.5, 2.5),
                       "3" = c(3.5, 3.5),
                       "4" = c(3.5, 3.5))
  if(dist == "poisson"){
    count[1] <- rpois(1, N1)
    if(count[1] == 0){ 
      count[1] = 1 
    }
    for(t in 2:len){
      prob <- discretize_gamma(1:(t-1), gamma_pars[1], gamma_pars[2])
      w[t] <- sum(rev(prob) * count[1:(t-1)])
      incidence[t] <- Rt[t] * w[t]
      count[t] <- rpois(1, incidence[t])
    }
  } else if (dist == "NB") { # set overdispersion size=5
    size = 5
    count[1] <- rnbinom(1, mu = N1, size = size)
    if(count[1] == 0){ 
      count[1] = 1 
    }
    for(t in 2:len){
      prob <- discretize_gamma(1:(t-1), gamma_pars[1], gamma_pars[2])
      w[t] <- sum(rev(prob) * count[1:(t-1)])
      incidence[t] <- Rt[t] * w[t]
      count[t] <- rnbinom(1, mu = incidence[t], size = size)
    }
  }
  lst <- list() 
  lst[["y"]] <- count
  lst[["gamma_pars"]] <- gamma_pars
  lst[["w"]] <- w
  return(lst)
}
