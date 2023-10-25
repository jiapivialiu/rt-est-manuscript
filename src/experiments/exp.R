# load functions and packages ----
library(here)
source("src/experiments/load_functions.R")
source("src/experiments/generate_data.R")
source("src/experiments/design_algos.R")

# create experiments ----
#rt_exp = makeExperimentRegistry("src/experiments/rt_exp5", seed = 525)
rt_exp = loadRegistry("src/experiments/rt_exp5", writeable = T)
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
addAlgorithm(name = "rtestim", fun = problem_solver)
addAlgorithm(name = "epilps", fun = problem_solver)

# add or remove experiments ----
addExperiments(problem_designs, algo_designs, repls = 1, combine = 'crossprod')
summarizeExperiments(by = c("Rt_case", "dist", "method"))

#removeExperiments(ids = 1:1600)
#summarizeExperiments()

# test before submitting ----
source("src/experiments/tests.R")

# getting system running time during running jobs ----
getStatus()

submitJobs()

# get reduced results ----
res <- ijoin(
  getJobPars(ids=findJobs()$job.id),
  reduceResultsDataTable(ids=findJobs()$job.id,fun = function(x) list(res_list = x))
)
for(i in 1:nrow(res)){
  res$result[[i]] <- res$result[[i]]$res_list
}
Rt_no_result <- unwrap(res[,-"result"])#, sep = "."
Rt_no_result
res_no_rtestim <- unwrap(res[(res$algorithm!="rtestim" | Rt_no_result$Rt_case!=2), ])

my_list <- res[Rt_no_result$Rt_case==2,][algorithm=="rtestim",]$result
combined_dat <- lapply(my_list, function(component) {
  data.frame(
    runtime = component$runtime, 
    Rt_kl = component$Rt_kl[[1]], 
    Rt_kl2 = component$Rt_kl[[2]],
    Rt_kl_month = component$Rt_kl_month[[1]],
    Rt_kl_month2 = component$Rt_kl_month[[2]])
})
res_rtestim <- do.call(rbind, combined_dat)
res_rtestim <- data.table(
  Rt_no_result[Rt_no_result$Rt_case==2,][algorithm=="rtestim",],
  res_rtestim
)

Rt_result <- full_join(res_no_rtestim, res_rtestim)

saveRDS(Rt_result, "src/experiments/rt_exp5_results.RDS")
