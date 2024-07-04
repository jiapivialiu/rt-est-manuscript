library(here)
source("load_functions.R")
source("generate_data.R")
source("design_algos.R")

rt_exp_cluster = makeExperimentRegistry(
  file.dir = here::here("rt_exp_cluster"), seed = 611,
  packages = c("EpiEstim", "rtestim", "EpiLPS", "EpiNow2", "data.table", "dplyr",
               "tidyr", "testthat", "batchtools", "microbenchmark"),
  source = c(here::here("generate_data.R"), here::here("design_algos.R"))
)

# create experiments ----
addProblem(name = "prob_design_meas", fun = data_generator, cache = TRUE)
addProblem(name = "prob_design_sars", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design_meas", fun = problem_solver)
addAlgorithm(name = "algo_design_sars", fun = problem_solver)
addAlgorithm(name = "algo_design_meas_ss", fun = problem_solver)
addExperiments(prob_list_meas, algo_list_meas, repls = 50, combine = 'crossprod')
addExperiments(prob_list_sars, algo_list_sars, repls = 50, combine = 'crossprod')
addExperiments(prob_list_meas, algo_list_sars, repls = 50, combine = 'crossprod')
addExperiments(prob_list_meas, algo_list_meas_ss, repls = 1, combine = 'crossprod')

addProblem(name = "prob_design_flu", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design_flu", fun = problem_solver)
addAlgorithm(name = "algo_design_flu_ss", fun = problem_solver)
addAlgorithm(name = "algo_design_meas_all", fun = problem_solver)
addExperiments(prob_list_flu, algo_list_flu, repls = 50, combine = 'crossprod')
addExperiments(prob_list_flu, algo_list_flu_ss, repls = 50, combine = 'crossprod')
addExperiments(prob_list_flu, algo_list_meas_all, repls = 50, combine = 'crossprod')

addProblem(name = "prob_design_epinow2", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design_epinow2", fun = problem_solver)
addExperiments(prob_list_epinow2, algo_list_epinow2, repls = 1, combine = 'crossprod')

jobs_meas_meas <- findExperiments(prob.name = "prob_design_meas", algo.name = "algo_design_meas")
jobs_sars_sars <- findExperiments(prob.name = "prob_design_sars", algo.name = "algo_design_sars")
jobs_meas_sars <- findExperiments(prob.name = "prob_design_meas", algo.name = "algo_design_sars")
jobs_meas_meass <- findExperiments(prob.name = "prob_design_meas", algo.name = "algo_design_meas_ss")
jobs_flu_flu <- findExperiments(prob.name = "prob_design_flu", algo.name = "algo_design_flu")
jobs_flu_meas <- findExperiments(prob.name = "prob_design_flu", algo.name = "algo_design_meas_all")
jobs_flu_flus <- findExperiments(prob.name = "prob_design_flu", algo.name = "algo_design_flu_ss")
jobepi2 <- findExperiments(prob.name = "prob_design_epinow2")

## summarize all experiments ----
summarizeExperiments(by = c("Rt_case", "dist", "si_type", "si_pars", "method"))

getStatus()

submitJobs(jobs_meas_meas, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_sars_sars, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_meas_sars, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_meas_meass, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_flu_flu, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_flu_meas, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_flu_flus, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobepi2, resources = list(ncpus = 1, walltime = "24:00:00", memory = "128G")) 

## save reduced results ----
res <- ijoin(
  getJobPars(ids=findDone()),
  reduceResultsDataTable(ids=findDone(), fun = function(x) list(res_list = x))
)
for(i in 1:nrow(res)){
  res$result[[i]] <- res$result[[i]]$res_list
}
Rt_result <- unwrap(res)
saveRDS(Rt_result, "rt_cluster_results.RDS")
