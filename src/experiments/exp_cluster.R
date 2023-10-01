# load functions and packages ----
library(here)
source("load_functions.R")
source("generate_data.R")
source("design_algos.R")

# create experiments ----
rt_pilot_exp = makeExperimentRegistry("rt_exp", seed = 937)

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

# getting system running time during running jobs ----

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
saveRDS(Rt_result, "rt_results.RDS")
