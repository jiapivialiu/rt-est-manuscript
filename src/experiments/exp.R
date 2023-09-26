# load functions and packages ----
library(here)
source("src/experiments/load_functions.R")
source("src/experiments/generate_data.R")
source("src/experiments/design_algos.R")

# create experiments ----
#rt_pilot_exp = makeExperimentRegistry("src/experiments/rt_pilot_exp", seed = 1117)
#rt_pilot_exp$cluster.functions = makeClusterFunctionsMulticore(ncpus = 8)
rt_pilot_exp = loadRegistry("src/experiments/rt_pilot_exp", writeable = T)

# design experiments ----
addProblem(name = "pois_scenario1", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario2", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario3", data = NULL, fun = data_generator)
addProblem(name = "pois_scenario4", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario1", data = NULL, fun = data_generator, seed=1228)
addProblem(name = "NB_scenario2", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario3", data = NULL, fun = data_generator)
addProblem(name = "NB_scenario4", data = NULL, fun = data_generator)

addAlgorithm(name = "rt_solver", fun = problem_solver)

# add or remove experiments ----
addExperiments(problem_designs, algo_designs, repls = 10, combine = 'crossprod')
summarizeExperiments(by = c("Rt_case", "dist", "method"))
#removeExperiments(ids=45)
#summarizeExperiments()

# test before submitting ----
source("src/experiments/tests.R")

# getting system running time during running jobs ----
getStatus()
options(digits.secs = 6)
start_time <- Sys.time()

submitJobs()

options(digits.secs = 6)
end_time <- Sys.time()

total_time <- end_time - start_time # 2.5 mins for 3 replicates
saveRDS(total_time, "total_run_time.RDS")

getStatus()

# get reduced results ----
res <- ijoin(
  getJobPars(),
  reduceResultsDataTable(fun = function(x) list(res_list = x))
)
for(i in 1:nrow(res)){
  res$result[[i]] <- res$result[[i]]$res_list
}
Rt_result <- unwrap(res, sep = ".")

# show runtime
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Rt_result$prob.pars.Rt_case <- as.factor(Rt_result$prob.pars.Rt_case)
Rt_result$prob.pars.dist <- as.factor(Rt_result$prob.pars.dist)

# our rtestim only wins for Rt1 
## possible reason: CV is time consuming, especially for many lambdas
## Rt3 with NB incidence runs very long time. Why? 
runtime_fig <- Rt_result %>%
  group_by(prob.pars.dist, prob.pars.Rt_case, algo.pars.method) %>%
  ggplot(aes(x =prob.pars.Rt_case, y = result.runtime/1000)) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  facet_wrap(vars(prob.pars.dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Running time in milliseconds", x = "Rt cases") + 
  labs(color="Methods") +
  theme_bw()


## Note: Rt ratio only use Rt[8:300] as EpiEstim only have estimates for this
fig_logratio <- Rt_result %>%
  group_by(prob.pars.dist, prob.pars.Rt_case, algo.pars.method) %>%
  ggplot(aes(x =prob.pars.Rt_case, y = abs(result.Rt_ratio))) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  facet_wrap(vars(prob.pars.dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Averaged log of absolute Rt_ratio", x="Rt cases") + 
  labs(color="Methods") +
  theme_bw()

fig_logratio_long <- Rt_result %>%
  group_by(prob.pars.dist, prob.pars.Rt_case, algo.pars.method) %>%
  ggplot(aes(x =prob.pars.Rt_case, y = abs(result.Rt_ratio_long))) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  facet_wrap(vars(prob.pars.dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Averaged log of absolute Rt_ratio including the first week", x="Rt cases") +  
  labs(color="Methods") +
  theme_bw()

fig_kl <- Rt_result %>%
  group_by(prob.pars.dist, prob.pars.Rt_case, algo.pars.method) %>%
  ggplot(aes(x =prob.pars.Rt_case, y = result.Rt_kl)) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  facet_wrap(vars(prob.pars.dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Averaged KL divergence", x="Rt cases") + 
  labs(color="Methods") +
  theme_bw()


fig_kl_long <- Rt_result %>%
  group_by(prob.pars.dist, prob.pars.Rt_case, algo.pars.method) %>%
  ggplot(aes(x =prob.pars.Rt_case, y = result.Rt_kl_long)) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  facet_wrap(vars(prob.pars.dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Averaged KL divergence including the first week", x="Rt cases") + 
  labs(color="Methods") +
  theme_bw()

#plot_ratio <- ggpubr::ggarrange(fig_logratio, fig_logratio_long, 
#                  fig_kl, fig_kl_long, nrow=2, nrol=2, common.legend=TRUE,
#                  legend="bottom")

ggsave(here::here("fig/runtime.png"), runtime_fig)
ggsave(here::here("fig/logratio.png"), fig_logratio)
ggsave(here::here("fig/logratio_long.png"), fig_logratio_long)
ggsave(here::here("fig/kl.png"), fig_kl)
ggsave(here::here("fig/kl_long.png"), fig_kl_long)


