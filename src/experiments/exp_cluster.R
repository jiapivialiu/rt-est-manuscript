# load functions and packages ----
library(here)
source("load_functions.R")
source("generate_data.R")
source("design_algos.R")

# create experiments ----
funfile <- list.files(here::here(), pattern = "[R]")
makeExperimentRegistry(
  "rt_exp2", 
  packages = c("EpiEstim", "rtestim", "EpiLPS", "data.table", "dplyr", 
               "tidyr", "testthat", "batchtools", "microbenchmark"), 
  source = c(here::here(funfile))
  )

library(batchtools)
rt_exp = loadRegistry("rt_exp2", writeable = T)

# design experiments ----
addProblem(name = "pois_scenario1", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario2", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario3", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario4", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario1", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario2", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario3", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario4", data = NULL, fun = data_generator)

addAlgorithm(name = "epiestim_week", fun = problem_solver)
addAlgorithm(name = "epiestim_month", fun = problem_solver)
addAlgorithm(name = "rtestim", fun = problem_solver)
addAlgorithm(name = "epilps", fun = problem_solver)

# add or remove experiments ----
addExperiments(problem_designs, algo_designs, repls = 50, combine = 'crossprod')

#exp_num <- nrow(summarizeExperiments(by = c("Rt_case", "dist", "method")))
#saveRDS(exp_num, "total_exp_number.RDS")
# getting system running time during running jobs ----
job.ids <- unwrap(getJobPars()) %>% 
  select(job.id, problem, algorithm) %>% 
  mutate(pa = paste(problem, algorithm)) %>%
  mutate(chunk = as.integer(as.factor(pa))) %>%
  select(-problem:-pa)

submitJobs(findNotDone(), resources = list(ncpus=1, walltime="24:00:00", memory="32G"))
#getStatus()

# get reduced results ----
res <- ijoin(
  getJobPars(),
  reduceResultsDataTable(fun = function(x) list(res_list = x))
)
#for(i in 1:nrow(res)){
#  res$result[[i]] <- res$result[[i]]$res_list
#}
saveRDS(res, "results.RDS")
