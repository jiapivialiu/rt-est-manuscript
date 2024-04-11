library(here)
source("load_functions.R")
source("generate_data.R")
source("design_algos.R")
rt_exp_cluster611 = loadRegistry("rt_exp_cluster611", writeable = T)

#rt_exp_cluster611 = makeExperimentRegistry(
#  file.dir = here::here("src/exp_cluster/rt_exp_cluster611"), seed = 611,
#  packages = c("EpiEstim", "rtestim", "EpiLPS", "EpiNow2", "data.table", "dplyr",
#               "tidyr", "testthat", "batchtools", "microbenchmark"),
#  source = c(here::here("src/exp_cluster/generate_data.R"), here::here("src/exp_cluster/design_algos.R"))
#)

#rt_exp_cluster450 = loadRegistry("rt_exp_cluster450", writeable = T)
#rt_exp_cluster1016 = loadRegistry("rt_exp_cluster1016", writeable = T)

addProblem(name = "prob_design", fun = data_generator, cache = TRUE)
addProblem(name = "prob_design_short", fun = data_generator, cache = TRUE)

addAlgorithm(name = "algo_design", fun = problem_solver)
addAlgorithm(name = "algo_design_all", fun = problem_solver)

addExperiments(prob_list, algo_list, repls = 50, combine = 'crossprod')
addExperiments(prob_list2, algo_list2, repls = 50, combine = 'crossprod')

# remove a few cases 
## exclude EpiNow2 for long epidemics: 
#removeExperiments(findExperiments(prob.pars = (len == 300), algo.pars = (method == "EpiNow2")))
## exclude misspecified SARS SI for true SARS samples: 
removeExperiments(findExperiments(prob.pars = (si_type == "SARS"), algo.pars = (si_pars == "measles")))

## summarize all experiments 
summarizeExperiments(findErrors(), by = c("Rt_case", "dist", "si_type", "si_pars", "method"))
summarizeExperiments()

getStatus()

submitJobs(findErrors(), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))


# split jobs into 8*4=32 piles, each pile include 2 dist * 8/9 methods * 50 replicates ----

# measles for long epidemics: 2 dist * 4 Rt * 8 algo * 5 replicates = 64 * 5 experiments 
## RtEstim * 4
jobs11 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs12 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
## other methods * 4
jobs13 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs14 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs11[[1]][1]; jobs12[[1]][1]; jobs13[[1]][1]; jobs14[[1]][1]

# SARS for long: 2 dist * 4 Rt * 8 algo * 5 replicates = 320 experiments 
jobs21 <- findExperiments(prob.pars = (len == 300 && si_type == "SARS" && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs22 <- findExperiments(prob.pars = (len == 300 && si_type == "SARS" && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs23 <- findExperiments(prob.pars = (len == 300 && si_type == "SARS" && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs24 <- findExperiments(prob.pars = (len == 300 && si_type == "SARS" && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs21[[1]][1]; jobs22[[1]][1]; jobs23[[1]][1]; jobs24[[1]][1] 

# measles solved by SARS for long: 2 dist * 4 Rt * 8 algo * 5 replicates = 320 experiments 
jobs31 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs32 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs33 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs34 <- findExperiments(prob.pars = (len == 300 && si_type == "measles" && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs31[[1]][1]; jobs32[[1]][1]; jobs33[[1]][1]; jobs34[[1]][1] 

#unique(rbind(jobs11,jobs12,jobs13,jobs14, jobs21,jobs22,jobs23,jobs24, jobs31,jobs32,jobs33,jobs34))

# measles for short: 2 dist * 1 Rt * 9 algo * 5 replicates = 90 
jobs41 <- findExperiments(prob.pars = (len == 50 && si_type == "flu" && dist == "poisson"), algo.pars = (si_pars == "flu" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs42 <- findExperiments(prob.pars = (len == 50 && si_type == "flu" && dist == "NB"), algo.pars = (si_pars == "flu" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs43 <- findExperiments(prob.pars = (len == 50 && si_type == "flu" && dist == "poisson"), algo.pars = (si_pars == "flu" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2")))
jobs44 <- findExperiments(prob.pars = (len == 50 && si_type == "flu" && dist == "NB"), algo.pars = (si_pars == "flu" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2")))
jobs41[[1]][1]; jobs42[[1]][1]; jobs43[[1]][1]; jobs44[[1]][1]

# SARS for short 
jobs51 <- findExperiments(prob.pars = (len == 50 && si_type == "flu" && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs52 <- findExperiments(prob.pars = (len == 50 && si_type == "flu" && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs53 <- findExperiments(prob.pars = (len == 50 && si_type == "flu" && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2")))
jobs54 <- findExperiments(prob.pars = (len == 50 && si_type == "flu" && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2")))
jobs51[[1]][1]; jobs52[[1]][1]; jobs53[[1]][1]; jobs54[[1]][1]

#unique(rbind(jobs41,jobs42,jobs43,jobs44, jobs51,jobs52,jobs53,jobs54,
#             jobs11,jobs12,jobs13,jobs14, jobs21,jobs22,jobs23,jobs24, jobs31,jobs32,jobs33,jobs34))

# submit jobs ---- 
summarizeExperiments(setdiff(jobs11, findDone()), by = c("Rt_case", "dist", "si_type", "method", "len"))

submitJobs(setdiff(jobs11, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs12, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs13, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs14, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

submitJobs(setdiff(jobs21, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs22, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs23, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs24, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

submitJobs(setdiff(jobs31, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs32, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs33, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs34, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

submitJobs(setdiff(jobs41, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs42, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs43, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs44, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

submitJobs(setdiff(jobs51, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs52, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs53, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(setdiff(jobs54, findDone()), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))


getStatus()


# get IDs of the first 20 replicates----
summarizeExperiments(jobs34_first20, by = c("Rt_case", "dist", "si_type", "method", "len"))

jobs11_first20 <- rbind(jobs11[1:80], jobs11[81:95], jobs11[126:140], jobs11[171:185], jobs11[216:230], 
                        jobs11[261:275], jobs11[306:320], jobs11[351:365], jobs11[396:410], 
                        jobs11[441:455], jobs11[486:500], jobs11[531:545], jobs11[576:590],
                        jobs11[621:635], jobs11[666:680], jobs11[711:725], jobs11[756:770])
jobs12_first20 <- rbind(jobs12[1:80], jobs12[81:95], jobs12[126:140], jobs12[171:185], jobs12[216:230], 
                        jobs12[261:275], jobs12[306:320], jobs12[351:365], jobs12[396:410], 
                        jobs12[441:455], jobs12[486:500], jobs12[531:545], jobs12[576:590],
                        jobs12[621:635], jobs12[666:680], jobs12[711:725], jobs12[756:770])
jobs13_first20 <- rbind(jobs13[1:80], jobs13[81:95], jobs13[126:140], jobs13[171:185], jobs13[216:230], 
                        jobs13[261:275], jobs13[306:320], jobs13[351:365], jobs13[396:410], 
                        jobs13[441:455], jobs13[486:500], jobs13[531:545], jobs13[576:590],
                        jobs13[621:635], jobs13[666:680], jobs13[711:725], jobs13[756:770])
jobs14_first20 <- rbind(jobs14[1:80], jobs14[81:95], jobs14[126:140], jobs14[171:185], jobs14[216:230], 
                        jobs14[261:275], jobs14[306:320], jobs14[351:365], jobs14[396:410], 
                        jobs14[441:455], jobs14[486:500], jobs14[531:545], jobs14[576:590],
                        jobs14[621:635], jobs14[666:680], jobs14[711:725], jobs14[756:770])


jobs21_first20 <- rbind(jobs21[1:80], jobs21[81:95], jobs21[126:140], jobs21[171:185], jobs21[216:230], 
                        jobs21[261:275], jobs21[306:320], jobs21[351:365], jobs21[396:410], 
                        jobs21[441:455], jobs21[486:500], jobs21[531:545], jobs21[576:590],
                        jobs21[621:635], jobs21[666:680], jobs21[711:725], jobs21[756:770])
jobs22_first20 <- rbind(jobs22[1:80], jobs22[81:95], jobs22[126:140], jobs22[171:185], jobs22[216:230], 
                        jobs22[261:275], jobs22[306:320], jobs22[351:365], jobs22[396:410], 
                        jobs22[441:455], jobs22[486:500], jobs22[531:545], jobs22[576:590],
                        jobs22[621:635], jobs22[666:680], jobs22[711:725], jobs22[756:770])
jobs23_first20 <- rbind(jobs23[1:80], jobs23[81:95], jobs23[126:140], jobs23[171:185], jobs23[216:230], 
                        jobs23[261:275], jobs23[306:320], jobs23[351:365], jobs23[396:410], 
                        jobs23[441:455], jobs23[486:500], jobs23[531:545], jobs23[576:590],
                        jobs23[621:635], jobs23[666:680], jobs23[711:725], jobs23[756:770])
jobs24_first20 <- rbind(jobs24[1:80], jobs24[81:95], jobs24[126:140], jobs24[171:185], jobs24[216:230], 
                        jobs24[261:275], jobs24[306:320], jobs24[351:365], jobs24[396:410], 
                        jobs24[441:455], jobs24[486:500], jobs24[531:545], jobs24[576:590],
                        jobs24[621:635], jobs24[666:680], jobs24[711:725], jobs24[756:770])

jobs31_first20 <- rbind(jobs31[1:80], jobs31[81:95], jobs31[126:140], jobs31[171:185], jobs31[216:230], 
                        jobs31[261:275], jobs31[306:320], jobs31[351:365], jobs31[396:410], 
                        jobs31[441:455], jobs31[486:500], jobs31[531:545], jobs31[576:590],
                        jobs31[621:635], jobs31[666:680], jobs31[711:725], jobs31[756:770])
jobs32_first20 <- rbind(jobs32[1:80], jobs32[81:95], jobs32[126:140], jobs32[171:185], jobs32[216:230], 
                        jobs32[261:275], jobs32[306:320], jobs32[351:365], jobs32[396:410], 
                        jobs32[441:455], jobs32[486:500], jobs32[531:545], jobs32[576:590],
                        jobs32[621:635], jobs32[666:680], jobs32[711:725], jobs32[756:770])
jobs33_first20 <- rbind(jobs33[1:80], jobs33[81:95], jobs33[126:140], jobs33[171:185], jobs33[216:230], 
                        jobs33[261:275], jobs33[306:320], jobs33[351:365], jobs33[396:410], 
                        jobs33[441:455], jobs33[486:500], jobs33[531:545], jobs33[576:590],
                        jobs33[621:635], jobs33[666:680], jobs33[711:725], jobs33[756:770])
jobs34_first20 <- rbind(jobs34[1:80], jobs34[81:95], jobs34[126:140], jobs34[171:185], jobs34[216:230], 
                        jobs34[261:275], jobs34[306:320], jobs34[351:365], jobs34[396:410], 
                        jobs34[441:455], jobs34[486:500], jobs34[531:545], jobs34[576:590],
                        jobs34[621:635], jobs34[666:680], jobs34[711:725], jobs34[756:770])





# I. Re-submit expired experiments ----
summarizeExperiments(jobs11, by = c("Rt_case", "dist", "si_type", "si_pars", "method"))
summarizeExperiments(jobs11)
submitJobs(findErrors(), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(findExpired(), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

jobsNotDone
saveRDS(jobs_to_remove, "dat/jobsToRemove.RDS")

summarizeExperiments(jobsNotDone, by = "Rt_case")
summarizeExperiments(jobsNotDone[1], by = c("Rt_case", "dist", "si_type", "si_pars", "method"))

removeExperiments(findErrors())
removeExperiments(findExpired())

## 1. re-submit long epidemics with Rt=1,2,3 and si_type="measles" for 50 replicates ----
addProblem(name = "prob_design_add", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design", fun = problem_solver)
addExperiments(prob_list_add, algo_list, repls = 50, combine = 'crossprod')
#jobs_add <- findExperiments(prob.name = "prob_design_add")
jobs_add_rt1_1 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 1 && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt1_2 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 1 && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt1_3 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 1 && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt1_4 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 1 && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt1_5 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 1 && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt1_6 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 1 && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt1_7 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 1 && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt1_8 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 1 && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))

jobs_add_rt2_1 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 2 && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt2_2 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 2 && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt2_3 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 2 && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt2_4 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 2 && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt2_5 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 2 && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt2_6 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 2 && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt2_7 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 2 && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt2_8 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 2 && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))

jobs_add_rt3_1 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 3 && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt3_2 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 3 && dist == "poisson"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt3_3 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 3 && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt3_4 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 3 && dist == "poisson"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt3_5 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 3 && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt3_6 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 3 && dist == "NB"), algo.pars = (si_pars == "SARS" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_add_rt3_7 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 3 && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_add_rt3_8 <- findExperiments(prob.name = "prob_design_add", prob.pars = (Rt_case == 3 && dist == "NB"), algo.pars = (si_pars == "measles" && method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
unique(rbind(jobs_add_rt1_1, jobs_add_rt2_1, jobs_add_rt3_1, 
             jobs_add_rt1_2, jobs_add_rt2_2, jobs_add_rt3_2,
             jobs_add_rt1_3, jobs_add_rt2_3, jobs_add_rt3_3, 
             jobs_add_rt1_4, jobs_add_rt2_4, jobs_add_rt3_4,
             jobs_add_rt1_5, jobs_add_rt2_5, jobs_add_rt3_5, 
             jobs_add_rt1_6, jobs_add_rt2_6, jobs_add_rt3_6,
             jobs_add_rt1_7, jobs_add_rt2_7, jobs_add_rt3_7, 
             jobs_add_rt1_8, jobs_add_rt2_8, jobs_add_rt3_8)
       )
# 200 * 8 * 3 = 4800 experiments
submitJobs(jobs_add_rt3_1, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_add_rt3_2, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_add_rt3_3, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_add_rt3_4, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

submitJobs(jobs_add_rt3_5, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_add_rt3_6, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_add_rt3_7, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_add_rt3_8, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))


## 2. re-submit a few with Rt=4, n=300: 80 exp's ----
addProblem(name = "prob_design_add2", fun = data_generator, cache = TRUE)
addProblem(name = "prob_design_add3", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design", fun = problem_solver)
addExperiments(prob_list_add2, algo_list, repls = 1, combine = 'crossprod')
addExperiments(prob_list_add3, algo_list, repls = 4, combine = 'crossprod')

jobsAdd2 <- findExperiments(prob.name = "prob_design_add2") # 2 si_pars * 8 methods = 16 exp's
jobsAdd3 <- findExperiments(prob.name = "prob_design_add3") # 4 rep's * 16 = 64 exp's
submitJobs(jobsAdd2, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobsAdd3, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))


## 3. re-submit a few short epidemics ----
addProblem(name = "prob_design_si_add1", fun = data_generator, cache = TRUE)
addProblem(name = "prob_design_si_add2", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design_all", fun = problem_solver)
addExperiments(prob_list_add_si1, algo_list2, repls = 3, combine = 'crossprod')
addExperiments(prob_list_add_si2, algo_list2, repls = 7, combine = 'crossprod')
jobsSIAdd1 <- findExperiments(prob.name = "prob_design_si_add1") # 3 rep's * 9 methods * 2 si_pars = 54 exp's
jobsSIAdd2 <- findExperiments(prob.name = "prob_design_si_add2") # 7 rep's * 9 methods * 2 si_pars = 126 exp's
submitJobs(jobsSIAdd1, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobsSIAdd2, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))



# II. Add more experiments for SI misspecification with slight deviation from the true: 3200 exp's ----
## 1. long epidemics: 3200 exp's ----
addProblem(name = "prob_design_si", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design_si", fun = problem_solver)
addExperiments(prob_list_mis_si, algo_list_mis_si, repls = 50, combine = 'crossprod')
#jobs4 <- findExperiments(prob.name = "prob_design_si")
jobs_new_si_rt1_1 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 1 && dist == "poisson"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_new_si_rt1_2 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 1 && dist == "poisson"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_new_si_rt1_3 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 1 && dist == "NB"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_new_si_rt1_4 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 1 && dist == "NB"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))

jobs_new_si_rt2_1 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 2 && dist == "poisson"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_new_si_rt2_2 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 2 && dist == "poisson"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_new_si_rt2_3 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 2 && dist == "NB"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_new_si_rt2_4 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 2 && dist == "NB"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))

jobs_new_si_rt3_1 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 3 && dist == "poisson"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_new_si_rt3_2 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 3 && dist == "poisson"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_new_si_rt3_3 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 3 && dist == "NB"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_new_si_rt3_4 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 3 && dist == "NB"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))

jobs_new_si_rt4_1 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 4 && dist == "poisson"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_new_si_rt4_2 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 4 && dist == "poisson"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_new_si_rt4_3 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 4 && dist == "NB"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter")))
jobs_new_si_rt4_4 <- findExperiments(prob.name = "prob_design_si", prob.pars = (Rt_case == 4 && dist == "NB"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))

unique(rbind(jobs_new_si_rt1_1, jobs_new_si_rt1_2, jobs_new_si_rt1_3, jobs_new_si_rt1_4, 
             jobs_new_si_rt2_1, jobs_new_si_rt2_2, jobs_new_si_rt2_3, jobs_new_si_rt2_4,
             jobs_new_si_rt3_1, jobs_new_si_rt3_2, jobs_new_si_rt3_3, jobs_new_si_rt3_4,
             jobs_new_si_rt4_1, jobs_new_si_rt4_2, jobs_new_si_rt4_3, jobs_new_si_rt4_4))
## 200 * 4 * 4 = 3200 experiments
submitJobs(jobs_new_si_rt4_1, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_new_si_rt4_2, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_new_si_rt4_3, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_new_si_rt4_4, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

## 2. short epidemics for mild SI misspecification: 900 exp's ----
addProblem(name = "prob_design_si_short", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design_si_short", fun = problem_solver)
addExperiments(prob_list_mis_si_short, algo_list_mis_si_short, repls = 50, combine = 'crossprod')
## 50 data * 1 Rt * 2 dist * 9 methods = 900 exp's
jobs_new_si_short_rt1 <- findExperiments(prob.name = "prob_design_si_short", prob.pars = (dist == "poisson"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2")))
jobs_new_si_short_rt2 <- findExperiments(prob.name = "prob_design_si_short", prob.pars = (dist == "poisson"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
jobs_new_si_short_rt3 <- findExperiments(prob.name = "prob_design_si_short", prob.pars = (dist == "NB"), algo.pars = (method %in% c("EpiEstim(week)", "EpiEstim(month)", "EpiLPS", "EpiFilter", "EpiNow2")))
jobs_new_si_short_rt4 <- findExperiments(prob.name = "prob_design_si_short", prob.pars = (dist == "NB"), algo.pars = (method %in% c("RtEstim(k=0)", "RtEstim(k=1)", "RtEstim(k=2)", "RtEstim(k=3)")))
unique(rbind(jobs_new_si_short_rt1, jobs_new_si_short_rt2, 
             jobs_new_si_short_rt3, jobs_new_si_short_rt4))
submitJobs(jobs_new_si_short_rt1, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_new_si_short_rt2, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_new_si_short_rt3, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))
submitJobs(jobs_new_si_short_rt4, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

# III. Run one experiment for long epidemic using EpiNow2 and get the running time ----
addProblem(name = "prob_design_epinow2", fun = data_generator, cache = TRUE)
addAlgorithm(name = "algo_design_epinow2", fun = problem_solver)
addExperiments(prob_list_epinow2, algo_list_epinow2, repls = 1, combine = 'crossprod')
jobs_epinow2 <- findExperiments(prob.name = "prob_design_epinow2")
submitJobs(jobs_epinow2, resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))


## check the results ----
# remove the exp's in `jobs_to_remove`
jobs_to_remove <- readRDS("jobsToRemove.RDS")
# also exclude the Rt=1,2,3 for long epidemics 
oldJobs_to_exclude <- findExperiments(prob.name = "prob_design", prob.pars = (Rt_case != 4 && si_type == "measles"))
jobs_total <- findExperiments(prob.name = "prob_design")
jobs_first <- setdiff(jobs_total, oldJobs_to_exclude)
jobs_first <- setdiff(jobs_first[[1]], jobs_to_remove)
jobs_second <- findExperiments(prob.name = "prob_design_add")
jobs_third <- findExperiments(prob.name = "prob_design_add2") # 2 si_pars * 8 methods = 16 exp's
jobs_fourth <- findExperiments(prob.name = "prob_design_add3") # 4 rep's * 16 = 64 exp's
jobs_to_download <- c(jobs_first, jobs_second[[1]], jobs_third[[1]], jobs_fourth[[1]])
#length(jobs_to_download) # 9600 in total
getStatus(jobs_to_download)

getStatus(jobs_second)

submitJobs(setdiff(jobs_to_download, findDone()[[1]]), resources = list(ncpus = 1, walltime = "8:00:00", memory = "32G"))

# another pile for SI: 3200 exp's
jobs_si <- findExperiments(prob.name = "prob_design_si")
getStatus(jobs_si)
jobs_si_short <- findExperiments(prob.name = "prob_design_si_short")
getStatus(jobs_si_short)

summarizeExperiments(findErrors(), by = c("Rt_case", "dist", "si_type", "si_pars", "method"))

res <- ijoin(
  getJobPars(ids=jobs_to_download),
  reduceResultsDataTable(ids=jobs_to_download, fun = function(x) list(res_list = x))
)
for(i in 1:nrow(res)){
  res$result[[i]] <- res$result[[i]]$res_list
}
Rt_result <- unwrap(res)

saveRDS(Rt_result, "rt_cluster_results611.RDS")

## 

Rt_result <- readRDS("dat/rt_cluster_results611.RDS")

cbPalette <- c("#E69F00","#F0E442", "#009E73", "#D55E00", 
               "#CC79A7", "#56B4E9", "#0072B2", "#999999")
Rt_result %>%
  filter(len == 300) %>% # 300
  filter(si_type == "SARS", si_pars == "SARS") %>% # SARS, measles?
  filter(method != "EpiEstim(month)") %>% #week
  filter(!is.na(Rt_kl)) %>%
  select(method, Rt_case, dist, Rt_kl) %>% #Rt_kl_month
  #mutate(Rt_kl = Rt_kl * len) %>%
  group_by(dist, method, Rt_case) %>%
  #summarize(Rt_mean = mean(Rt_kl)) %>% #Rt_kl_month
  ggplot(aes(x = method, y = Rt_kl)) + 
  geom_boxplot(aes(color = method)) +
  facet_grid(dist ~ Rt_case, scales = "free") +
  scale_colour_manual(values = cbPalette) +
  scale_y_log10() + 
  theme_bw()
# larger incidence? 
# true SARS, false measles gamma params?


Rt_result %>%
  #filter(len == 300) %>%
  #filter(si_type == "measles", si_pars == "measles") %>%
  filter(method != "EpiEstim(week)") %>%
  filter(method != "EpiEstim(month)") %>%
  filter(is.na(Rt_kl))

Rt_result



