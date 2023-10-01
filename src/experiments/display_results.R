# check job status 
findOnSystem()
findRunning()
waitForJobs()

findErrors()
ids <- findNotDone()
getErrorMessages()

removeExperiments(ids=ids$job.id)


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
  ggplot(aes(x =prob.pars.Rt_case, y = result.runtime/1e6)) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  facet_wrap(vars(prob.pars.dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Running time in seconds", x = "Rt cases") + 
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

fig_kl_pois <- Rt_result %>%
  filter(prob.pars.dist == "poisson") %>%
  group_by(prob.pars.Rt_case, algo.pars.method) %>%
  ggplot(aes(x = factor(prob.pars.Rt_case), y = result.Rt_kl)) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  #facet_wrap(vars(prob.pars.dist, prob.pars.Rt_case)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Averaged KL for Poisson incidences", x="Rt cases") +
  labs(color="Methods") +
  theme_bw()

fig_kl_nb <- Rt_result %>%
  filter(prob.pars.dist == "NB") %>%
  group_by(prob.pars.Rt_case, algo.pars.method) %>%
  ggplot(aes(x = factor(prob.pars.Rt_case), y = result.Rt_kl)) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  #facet_wrap(vars(prob.pars.dist, prob.pars.Rt_case)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Averaged KL for negative Binomial incidences", x="Rt cases") +
  labs(color="Methods") +
  theme_bw()


fig_kl_long <- Rt_result %>%
  group_by(prob.pars.dist, prob.pars.Rt_case, algo.pars.method) %>%
  ggplot(aes(x =prob.pars.Rt_case, y = result.Rt_kl_long)) +
  geom_boxplot(aes(col = algo.pars.method)) + 
  facet_wrap(vars(prob.pars.dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Averaged KL including the first week", x="Rt cases") + 
  labs(color="Methods") +
  theme_bw()

#plot_ratio <- ggpubr::ggarrange(fig_logratio, fig_logratio_long, 
#                  fig_kl, fig_kl_long, nrow=2, nrol=2, common.legend=TRUE,
#                  legend="bottom")

ggsave(here::here("fig/runtime.png"), runtime_fig)
ggsave(here::here("fig/logratio.png"), fig_logratio)
ggsave(here::here("fig/logratio_long.png"), fig_logratio_long)
ggsave(here::here("fig/kl_long.png"), fig_kl_long)

ggsave(here::here("fig/kl_pois.png"), fig_kl_pois, width = 5.63, height = 3.78)
ggsave(here::here("fig/kl_nb.png"), fig_kl_nb, width = 5.63, height = 3.78)


fig_kl <- ggarrange(fig_kl_pois, fig_kl_nb, ncol=2, nrow=1, 
                    common.legend = TRUE, legend = "bottom",
                    font.label = list(size = 14))
ggsave(here::here("fig/kl.png"), fig_kl, width = 6.67, height = 4.06)

