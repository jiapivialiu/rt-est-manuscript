# load functions and packages ----
library(here)
source("src/experiments/load_functions.R")
source("src/experiments/generate_data.R")
source("src/experiments/design_algos.R")

# create experiments ----
#rt_exp = makeExperimentRegistry("src/experiments/rt_exp", seed = 953)
rt_exp$cluster.functions = makeClusterFunctionsMulticore(ncpus = 8)
rt_exp = loadRegistry("src/experiments/rt_exp", writeable = T)

# design experiments ----
addProblem(name = "pois_scenario1", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario2", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario3", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario4", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario1", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario2", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario3", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario4", data = NULL, fun = data_generator)

addAlgorithm(name = "rt_solver", fun = problem_solver)

# add or remove experiments ----
addExperiments(problem_designs, algo_designs, repls = 50, combine = 'crossprod')
summarizeExperiments(by = c("Rt_case", "dist", "method"))

#removeExperiments(ids = )
#summarizeExperiments()

# test before submitting ----
source("src/experiments/tests.R")

# getting system running time during running jobs ----
getStatus()

submitJobs()

# get reduced results ----
res <- ijoin(
  getJobPars(),
  reduceResultsDataTable(fun = function(x) list(res_list = x))
)
for(i in 1:nrow(res)){
  res$result[[i]] <- res$result[[i]]$res_list
}
Rt_result <- unwrap(res, sep = ".")
saveRDS(Rt_result, "src/experiments/rt_full_results.RDS")
