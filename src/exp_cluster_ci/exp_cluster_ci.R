# load functions and packages ----
library(here)
source(here::here("load_functions.R"))
source(here::here("generate_data_ci.R"))
source(here::here("design_algos_ci.R"))

rt_exp_cluster_ci108 <- loadRegistry("rt_exp_cluster_ci108", writeable = T)
rt_exp_cluster_ci1145 <- loadRegistry("rt_exp_cluster_ci1145", writeable = T)


addProblem(name = "prob_design", fun = data_generator) 
addAlgorithm(name = "algo_design", fun = problem_solver) 
addExperiments(prob_list, algo_list, repls = 50, combine = 'crossprod')

summarizeExperiments(by = c("Rt_case", "dist", "si_type", "method"))
summarizeExperiments()

# split the jobs ----
## measles & poisson & RtEstim
jobs11 <- findExperiments(prob.pars = (si_type == "measles" && dist == "poisson" && Rt_case %in% 1:2), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs12 <- findExperiments(prob.pars = (si_type == "measles" && dist == "poisson" && Rt_case %in% 3:4), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
## measles & poisson & not RtEstim
jobs13 <- findExperiments(prob.pars = (si_type == "measles" && dist == "poisson" && Rt_case %in% 1:2), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs14 <- findExperiments(prob.pars = (si_type == "measles" && dist == "poisson" && Rt_case %in% 3:4), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))

## measles & NB & RtEstim
jobs15 <- findExperiments(prob.pars = (si_type == "measles" && dist == "NB" && Rt_case %in% 1:2), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs16 <- findExperiments(prob.pars = (si_type == "measles" && dist == "NB" && Rt_case %in% 3:4), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
## measles & NB & not RtEstim
jobs17 <- findExperiments(prob.pars = (si_type == "measles" && dist == "NB" && Rt_case %in% 1:2), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs18 <- findExperiments(prob.pars = (si_type == "measles" && dist == "NB" && Rt_case %in% 3:4), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))

## SARS 
jobs21 <- findExperiments(prob.pars = (si_type == "SARS" && dist == "poisson" && Rt_case %in% 1:2), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs22 <- findExperiments(prob.pars = (si_type == "SARS" && dist == "poisson" && Rt_case %in% 3:4), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs23 <- findExperiments(prob.pars = (si_type == "SARS" && dist == "poisson" && Rt_case %in% 1:2), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs24 <- findExperiments(prob.pars = (si_type == "SARS" && dist == "poisson" && Rt_case %in% 3:4), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))

jobs25 <- findExperiments(prob.pars = (si_type == "SARS" && dist == "NB" && Rt_case %in% 1:2), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs26 <- findExperiments(prob.pars = (si_type == "SARS" && dist == "NB" && Rt_case %in% 3:4), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs27 <- findExperiments(prob.pars = (si_type == "SARS" && dist == "NB" && Rt_case %in% 1:2), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs28 <- findExperiments(prob.pars = (si_type == "SARS" && dist == "NB" && Rt_case %in% 3:4), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))

unique(rbind(jobs21,jobs22,jobs23,jobs24, jobs25,jobs26,jobs27,jobs28,
             jobs11,jobs12,jobs13,jobs14, jobs15,jobs16,jobs17,jobs18))
      
# SUBMIT JOBS ----       
submitJobs(jobs11, resources = list(ncpus = 1, walltime = "8:00:00", memory = "16G"))


summarizeExperiments(findErrors(), by = c("Rt_case", "dist", "si_type", "method"))
summarizeExperiments(findExpired(), by = c("Rt_case", "dist", "si_type", "method"))

submitJobs(findErrors(), resources = list(ncpus = 1, walltime = "8:00:00", memory = "16G"))
submitJobs(findExpired(), resources = list(ncpus = 1, walltime = "8:00:00", memory = "16G"))

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

saveRDS(Rt_result, "rt_cluster_ci_results108.RDS")

# read results ----
#Rt_result <- readRDS("dat/rt_cluster_ci_results108.RDS")

cbPalette <- c("#E69F00","#F0E442", "#009E73", "#D55E00", 
               "#CC79A7", "#0072B2", "#999999", "#56B4E9")
# correct measles or SARS gamma_pars, no EpiNow2
# len=300 only
Rt_result <- Rt_result %>%
  mutate(method = fct_recode(method,
                             "EpiEstim (weekly)" = "EpiEstim(week)",
                             "EpiEstim (monthly)" = "EpiEstim(month)",
                             "RtEstim (k=0)" = "RtEstim(k=0)",
                             "RtEstim (k=1)" = "RtEstim(k=1)",
                             "RtEstim (k=2)" = "RtEstim(k=2)",
                             "RtEstim (k=3)" = "RtEstim(k=3)"
  )) %>%
  mutate(method = fct_relevel(
    method, "EpiEstim (weekly)", "EpiEstim (monthly)",
    "EpiLPS", "EpiFilter", "RtEstim (k=0)", "RtEstim (k=1)",
    "RtEstim (k=2)", "RtEstim (k=3)"
  )) %>%
  mutate(dist = fct_recode(dist,
                           "Poisson" = "poisson",
                           "Negative Binomial" = "NB"
  )) %>%
  mutate(dist = fct_relevel(dist, "Poisson", "Negative Binomial")) %>%
  mutate(Rt_case = as.character(Rt_case)) %>% 
  mutate(Rt_case = fct_recode(Rt_case,
                              "Scenario 1" = "1",
                              "Scenario 2" = "2",
                              "Scenario 3" = "3",
                              "Scenario 4" = "4"
  ))

# ci score
Rt_result %>% 
  #filter(method != "EpiEstim (monthly)") %>%
  filter(method != "EpiLPS") %>%
  filter(si_type == "SARS") %>%
  group_by(dist, Rt_case, method) %>%
  ggplot(aes(y = ci_score, x = method)) +
  facet_grid(dist ~ Rt_case, scales = "free") +
  geom_boxplot(aes(col = method)) +
  scale_colour_manual(values = cbPalette) +
  scale_y_log10() +
  labs(x = "Methods", y = "Interval scores") + 
  theme_bw() + 
  theme(axis.text.x = element_blank())

# ci percentage
Rt_result %>% 
  #filter(method != "EpiEstim(month)") %>%
  filter(method != "EpiLPS") %>%
  filter(si_type == "measles") %>%
  group_by(dist, Rt_case, method) %>%
  ggplot(aes(y = ci_percentage, x = method)) + 
  facet_grid(dist ~ Rt_case, scales = "free") +
  geom_boxplot(aes(col = method)) +
  scale_colour_manual(values = cbPalette) +
  #scale_y_log10() +
  geom_hline(yintercept = 0.95, color = "orange", linetype = "dashed") + 
  labs(x = "Methods", y = "CI percentages of coverage") + 
  theme_bw() + 
  theme(axis.text.x = element_blank())


# ci coverage averaged across all replicates
ci_len <- double(nrow(Rt_result))
for(i in 1:nrow(Rt_result)) {
  ci_len[i] <- length(Rt_result[i,]$ci_coverage[[1]])
}
Rt_result <- data.table(Rt_result, ci_len)

Rt_res_ci <- Rt_result %>%
  select(si_type, ci_len, dist, Rt_case, method, ci_coverage) %>%
  group_by(si_type, dist, Rt_case, method) %>%
  unnest_wider(ci_coverage, names_sep = "")

# replace NAs with negative values
columns_to_replace <- paste0("ci_coverage", 1:300)
Rt_res_ci_noNAs <- Rt_res_ci %>%
  mutate_at(vars(columns_to_replace), list(~ replace_na(., -1)))
Rt_res_ci_noNAs %>% 
  group_by(si_type, ci_len, dist, Rt_case, method) %>%
  summarize(across(all_of(columns_to_replace), mean)) %>%
  pivot_longer(cols = columns_to_replace, names_to = "index", values_to = "ci_coverage") %>%
  mutate(index = as.numeric(gsub("\\D", "", index))) %>%
  mutate(time_index = 300 - ci_len + index) %>%
  group_by(dist, method, Rt_case) %>%
  filter(ci_coverage >= 0) %>%
  filter(si_type == "measles", dist != "Negative Binomial") %>%
  #filter(!method %in% c("EpiEstim (monthly)", "EpiEstim (weekly)")) %>%
  filter(method %in% c("RtEstim (k=0)", "RtEstim (k=1)", "RtEstim (k=2)", "RtEstim (k=3)")) %>%
  ggplot(aes(y = ci_coverage, x = time_index)) +
  geom_line(aes(col = method)) + 
  facet_grid(dist ~ Rt_case, scales = "free") +
  scale_colour_manual(values = cbPalette) + 
  theme_bw()






