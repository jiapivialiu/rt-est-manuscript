# load functions and packages ----
library(here)
source(here::here("load_functions.R"))
source(here::here("generate_data_ci.R"))
source(here::here("design_algos_ci.R"))

rt_exp_cluster = makeExperimentRegistry(
  file.dir = here::here("rt_exp_cluster"), seed = 211,
  packages = c("EpiEstim", "rtestim", "EpiLPS", "data.table", "dplyr",
               "tidyr", "testthat", "batchtools", "microbenchmark"),
  source = c(here::here("generate_data_ci.R"), here::here("design_algos_ci.R"))
)
#loadRegistry("rt_exp_cluster", writeable = T)

addProblem(name = "prob_design", fun = data_generator) 
addAlgorithm(name = "algo_design", fun = problem_solver) 
addExperiments(prob_list, algo_list, repls = 50, combine = 'crossprod')

summarizeExperiments(by = c("Rt_case", "dist", "si_type", "method"))
summarizeExperiments()

# SUBMIT JOBS ----    
jobs <- findExperiments(prob.name = "prob_design")
submitJobs(jobs[1:80], resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

getStatus()

# save results ----
res <- ijoin(
  getJobPars(ids=findDone()$job.id),
  reduceResultsDataTable(ids=findDone()$job.id, fun = function(x) list(res_list = x))
)
for(i in 1:nrow(res)){
  res$result[[i]] <- res$result[[i]]$res_list
}
Rt_result <- unwrap(res)

saveRDS(Rt_result, "rt_cluster_ci_results.RDS")
