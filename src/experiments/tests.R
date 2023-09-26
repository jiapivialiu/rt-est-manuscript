## find a subset of experiments ----
id11 = findExperiments(prob.name = "pois_scenario1", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiEstim'))[1]
id12 = findExperiments(prob.name = "pois_scenario1", algo.name = 'rt_solver', 
                      algo.par = (method == 'rtestim'))[1]
id13 = findExperiments(prob.name = "pois_scenario1", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiLPS'))[1]

id21 = findExperiments(prob.name = "pois_scenario2", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiEstim'))[1]
id22 = findExperiments(prob.name = "pois_scenario2", algo.name = 'rt_solver', 
                      algo.par = (method == 'rtestim'))[1]
id23 = findExperiments(prob.name = "pois_scenario2", algo.name = 'rt_solver', 
                       algo.par = (method == 'EpiLPS'))[1]

id31 = findExperiments(prob.name = "pois_scenario3", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiEstim'))[1]
id32 = findExperiments(prob.name = "pois_scenario3", algo.name = 'rt_solver', 
                      algo.par = (method == 'rtestim'))[1]
id33 = findExperiments(prob.name = "pois_scenario3", algo.name = 'rt_solver', 
                       algo.par = (method == 'EpiLPS'))[1]

id41 = findExperiments(prob.name = "pois_scenario4", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiEstim'))[1]
id42 = findExperiments(prob.name = "pois_scenario4", algo.name = 'rt_solver', 
                      algo.par = (method == 'rtestim'))[1]
id43 = findExperiments(prob.name = "pois_scenario4", algo.name = 'rt_solver', 
                       algo.par = (method == 'EpiLPS'))[1]

id51 = findExperiments(prob.name = "NB_scenario1", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiEstim'))[1]
id52 = findExperiments(prob.name = "NB_scenario1", algo.name = 'rt_solver', 
                      algo.par = (method == 'rtestim'))[1]
id53 = findExperiments(prob.name = "NB_scenario1", algo.name = 'rt_solver', 
                       algo.par = (method == 'EpiLPS'))[1]

id61 = findExperiments(prob.name = "NB_scenario2", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiEstim'))[1]
id62 = findExperiments(prob.name = "NB_scenario2", algo.name = 'rt_solver', 
                      algo.par = (method == 'rtestim'))[1]
id63 = findExperiments(prob.name = "NB_scenario2", algo.name = 'rt_solver', 
                       algo.par = (method == 'EpiLPS'))[1]

id71 = findExperiments(prob.name = "NB_scenario3", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiEstim'))[1]
id72 = findExperiments(prob.name = "NB_scenario3", algo.name = 'rt_solver', 
                      algo.par = (method == 'rtestim'))[1]
id73 = findExperiments(prob.name = "NB_scenario3", algo.name = 'rt_solver', 
                       algo.par = (method == 'EpiLPS'))[1]

id81 = findExperiments(prob.name = "NB_scenario4", algo.name = 'rt_solver', 
                      algo.par = (method == 'EpiEstim'))[1]
id82 = findExperiments(prob.name = "NB_scenario4", algo.name = 'rt_solver', 
                      algo.par = (method == 'rtestim'))[1]
id83 = findExperiments(prob.name = "NB_scenario4", algo.name = 'rt_solver', 
                       algo.par = (method == 'EpiLPS'))[1]

## test jobs before submitting ----
testJob(id = id11$job.id)
testJob(id = id12$job.id)
testJob(id = id13$job.id)

testJob(id = id21$job.id)
testJob(id = id22$job.id)
testJob(id = id23$job.id)

testJob(id = id31$job.id)
testJob(id = id32$job.id)
testJob(id = id33$job.id)

testJob(id = id41$job.id)
testJob(id = id42$job.id)
testJob(id = id43$job.id)

testJob(id = id51$job.id)
testJob(id = id52$job.id)
testJob(id = id53$job.id)

testJob(id = id61$job.id)
testJob(id = id62$job.id)
testJob(id = id63$job.id)

testJob(id = id71$job.id)
testJob(id = id72$job.id)
testJob(id = id73$job.id)

testJob(id = id81$job.id)
testJob(id = id82$job.id)
testJob(id = id83$job.id)
