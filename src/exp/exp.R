# load functions and packages ----
library(here)
source(here::here("src/exp/load_functions.R"))
source("src/exp/generate_data.R")
source("src/exp/design_algos.R")

# create/load experiments ----
#rt_exp_all = makeExperimentRegistry("src/exp/rt_exp_all", seed = 715)
rt_exp_all = loadRegistry("src/exp/rt_exp_all", writeable = T)
rt_exp_all$cluster.functions = makeClusterFunctionsMulticore(ncpus = 8)

# design experiments ----
addProblem(name = "prob_design", data = NULL, fun = data_generator)

addAlgorithm(name = "algo_design", fun = problem_solver)

addExperiments(prob_list, algo_list, repls = 5, combine = 'crossprod')

# remove a few cases 
## exclude EpiNow2 for long epidemics: 
removeExperiments(findExperiments(prob.pars = (len == 300), algo.pars = (method == "EpiNow2")))
## exclude misspecified SARS SI for true SARS samples: 
removeExperiments(findExperiments(prob.pars = (si_type == "SARS"), algo.pars = (si_pars == "measles")))
# confirm:
findExperiments(prob.pars = (si_type == "SARS"), algo.pars = (si_pars == "measles"))


## summarize all experiments 
exp_ids <- summarizeExperiments(by = c("len", "dist", "si_type", "si_pars", "Rt_case", "method"))
exp_ids
summarizeExperiments()

# test before submitting ----
#source(here::here("src/exp/tests.R"))

# getting system running time during running jobs ----
getStatus()

# measles for long epidemics: 2 * 4 Rt * 8 algo = 64 experiments (* 1 replicate) (EpiNow2 not applied)
jobs11 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "poisson"), algo.pars = (si_pars == "measles"))
jobs12 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "NB"), algo.pars = (si_pars == "measles"))
# measles for short epidemics: 2 * 4 Rt * 9 algo = 72 experiments (* 1 replicate) (EpiNow2 applied)
jobs13 <- findExperiments(prob.pars = (len == 50 && si_type == "measles" && dist == "poisson"), algo.pars = (si_pars == "measles"))
jobs14 <- findExperiments(prob.pars = (len == 50 && si_type == "measles" && dist == "NB"), algo.pars = (si_pars == "measles"))
jobs11[[1]][1]; jobs12[[1]][1]; jobs13[[1]][1]; jobs14[[1]][1] # confirm no duplicates

# SARS for long: 2 * 4 Rt * 8 algo = 64 experiments
jobs21 <- findExperiments(prob.pars = (len == 300 && si_type == "SARS" && dist == "poisson"), algo.pars = (si_pars == "SARS"))
jobs22 <- findExperiments(prob.pars = (len == 300 && si_type == "SARS" && dist == "NB"), algo.pars = (si_pars == "SARS"))
# SARS for short: 36 * 2 = 72
jobs23 <- findExperiments(prob.pars = (len == 50 && si_type == "SARS" && dist == "poisson"), algo.pars = (si_pars == "SARS"))
jobs24 <- findExperiments(prob.pars = (len == 50 && si_type == "SARS" && dist == "NB"), algo.pars = (si_pars == "SARS"))
jobs21[[1]][1]; jobs22[[1]][1]; jobs23[[1]][1]; jobs24[[1]][1] # confirm no duplicates

# measles solved by SARS for long: 32 * 2 = 64
jobs31 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "poisson"), algo.pars = (si_pars == "SARS"))
jobs32 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "NB"), algo.pars = (si_pars == "SARS"))
# measles solved by SARS for short: 36 * 2 = 72
jobs33 <- findExperiments(prob.pars = (len == 50 && si_type == "measles" && dist == "poisson"), algo.pars = (si_pars == "SARS"))
jobs34 <- findExperiments(prob.pars = (len == 50 && si_type == "measles" && dist == "NB"), algo.pars = (si_pars == "SARS"))
jobs31[[1]][1]; jobs32[[1]][1]; jobs33[[1]][1]; jobs34[[1]][1] # confirm no duplicates

submitJobs(jobs11)
submitJobs(jobs12)
submitJobs(jobs13)
submitJobs(jobs14)

submitJobs(jobs21)
submitJobs(jobs22)
submitJobs(jobs23)
submitJobs(jobs24)

submitJobs(jobs31)
submitJobs(jobs32)
submitJobs(jobs33)
submitJobs(jobs34)


getErrorMessages()

removeExperiments(findErrors())


# get reduced results 
res <- ijoin(
  getJobPars(ids=findDone()$job.id),
  reduceResultsDataTable(ids=findDone()$job.id, fun = function(x) list(res_list = x))
)
for(i in 1:nrow(res)){
  res$result[[i]] <- res$result[[i]]$res_list
}
Rt_result <- unwrap(res)

saveRDS(Rt_result, "dat/rt_all_results.RDS")
Rt_result <- readRDS("dat/rt_all_results.RDS")
# 4Rt * 2dist * 8methods * 50samples

# ----
#Rt_result$Rt_case <- as.factor(Rt_result$Rt_case)
#Rt_result$dist <- as.factor(Rt_result$dist)
#Rt_result$method <- as.factor(Rt_result$method)
n <- dim(Rt_result)[1]
library(tidyverse)

cbPalette <- c("#E69F00","#F0E442", "#009E73", "#D55E00", 
               "#CC79A7", "#56B4E9", "#0072B2", "#999999")
Rt_result %>%
  filter(len == 300) %>%
  filter(si_type == "measles", si_pars == "SARS") %>%
  filter(method != "EpiEstim(month)") %>%
  select(method, Rt_case, dist, Rt_kl) %>%
  group_by(dist, method, Rt_case) %>%
  summarize(Rt_mean = mean(Rt_kl)) %>%
  ggplot(aes(x = method, y = Rt_mean)) + 
  geom_boxplot(aes(color = method)) +
  facet_grid(dist ~ Rt_case, scales = "free") +
  scale_colour_manual(values = cbPalette) +
  scale_y_log10() + 
  theme_bw()



