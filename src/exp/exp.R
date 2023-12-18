# load functions and packages ----
library(here)
source(here::here("src/exp/load_functions.R"))
source(here::here("src/exp/generate_data.R"))
source(here::here("src/exp/design_algos.R"))

# create/load experiments ----
#rt_exp = makeExperimentRegistry("src/exp/rt_exp", seed = 156)
rt_exp = loadRegistry("src/exp/rt_exp", writeable = T)
rt_exp$cluster.functions = makeClusterFunctionsMulticore(ncpus = 8)

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
addAlgorithm(name = "epilps", fun = problem_solver)
addAlgorithm(name = "rtestim0", fun = problem_solver)
addAlgorithm(name = "rtestim1", fun = problem_solver)
addAlgorithm(name = "rtestim3", fun = problem_solver)

addExperiments(problem_design1, algo_design1, repls = 50, combine = 'crossprod')
addExperiments(problem_design2, algo_design1, repls = 50, combine = 'crossprod')
addExperiments(problem_design3, algo_design1, repls = 50, combine = 'crossprod')
addExperiments(problem_design4, algo_design1, repls = 50, combine = 'crossprod')
# rtestim exp's
addExperiments(problem_design1, algo_design2, repls = 50, combine = 'crossprod')
addExperiments(problem_design2, algo_design3, repls = 50, combine = 'crossprod')
addExperiments(problem_design2, algo_design4, repls = 50, combine = 'crossprod')
addExperiments(problem_design3, algo_design3, repls = 50, combine = 'crossprod')
addExperiments(problem_design4, algo_design4, repls = 50, combine = 'crossprod')

## summarize all experiments 
summarizeExperiments(by = c("Rt_case", "dist", "method"))

# test before submitting ----
source(here::here("src/exp/tests.R"))

# getting system running time during running jobs ----
getStatus()

submitJobs()

# get reduced results ----
res <- ijoin(
  getJobPars(ids=findDone()$job.id),
  reduceResultsDataTable(ids=findDone()$job.id, fun = function(x) list(res_list = x))
)
for(i in 1:nrow(res)){
  res$result[[i]] <- res$result[[i]]$res_list
}
Rt_result <- unwrap(res)

saveRDS(Rt_result, "dat/rt_exp_results.RDS")
