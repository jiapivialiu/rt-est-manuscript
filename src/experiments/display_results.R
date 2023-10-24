
#res <- readRDS("src/experiments/results2.RDS")
#for(i in 1:nrow(res)){
#  res$result[[i]] <- res$result[[i]]$res_list
#}
#Rt_no_result <- unwrap(res[,-"result"])#, sep = "."
#Rt_no_result
#res_no_rtestim <- unwrap(res[(res$algorithm!="rtestim" | Rt_no_result$Rt_case!=2), ])
#
#my_list <- res[Rt_no_result$Rt_case==2,][algorithm=="rtestim",]$result
#combined_dat <- lapply(my_list, function(component) {
#  data.frame(runtime = component$runtime, 
#             Rt_kl = component$Rt_kl[[1]], 
#             Rt_kl2 = component$Rt_kl[[2]])
#})
#res_rtestim <- do.call(rbind, combined_dat)
#res_rtestim <- data.table(
#  Rt_no_result[Rt_no_result$Rt_case==2,][algorithm=="rtestim",],
#  res_rtestim
#)
#
#Rt_result <- full_join(res_no_rtestim, res_rtestim)


library(ggplot2)
# show runtime
cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#56B4E9", "#999999", 
               "#F0E442", "#D55E00", "#CC79A7")
Rt_result$Rt_case <- as.factor(Rt_result$Rt_case)
Rt_result$dist <- as.factor(Rt_result$dist)

# our rtestim only wins for Rt1 
## possible reason: CV is time consuming, especially for many lambdas
## Rt3 with NB incidence runs very long time. Why? 
runtime_fig <- Rt_result %>%
  group_by(dist, Rt_case, method) %>%
  ggplot(aes(x = Rt_case, y =runtime/1e6)) +
  geom_boxplot(aes(col = method)) + 
  facet_wrap(vars(dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Running time in seconds", x = "Rt scenarios") + 
  labs(color = "Methods") +
  theme_bw()

Rt_result_long <- Rt_result[, -c("Rt_kl2", "Rt_kl_month2")]
rt_expand <- Rt_result[is.na(Rt_result$Rt_kl2) == FALSE, -c("Rt_kl", "Rt_kl_month")]
names(rt_expand)[8] <- "Rt_kl"
names(rt_expand)[9] <- "Rt_kl_month"
rt_expand$method <- rep("RtEstim(k=1)", dim(rt_expand)[1])
Rt_result_long <- rbind(Rt_result_long, rt_expand)

fig_kl_pois <- Rt_result_long %>%
  filter(method != "EpiEstim(month)") %>%
  filter(dist == "poisson") %>%
  mutate(method = fct_recode(method, "EpiEstim" = "EpiEstim(week)")) %>% 
  group_by(Rt_case, method) %>%
  #filter(Rt_kl <= 3) %>%
  ggplot(aes(x = factor(Rt_case), y = Rt_kl)) +
  geom_boxplot(aes(col = method)) + 
  #facet_wrap(vars(prob.pars.dist, prob.pars.Rt_case)) + 
  scale_colour_manual(values = cbPalette) + 
  coord_cartesian(ylim = c(0, .3)) + # excluding 3 outliers (>3s) of EpiEstim (1 month, 2 week)
  scale_y_sqrt()+
  labs(y = "Averaged KL for Poisson incidences", x="Rt cases") +
  labs(color="Methods") +
  theme_bw()

fig_kl_nb <- Rt_result_long %>%
  filter(dist == "NB") %>%
  filter(method != "EpiEstim(month)") %>%
  mutate(method = fct_recode(method, 
                             "EpiEstim" = "EpiEstim(week)")) %>% 
  group_by(Rt_case, method) %>%
  filter(Rt_kl <= 3) %>%
  ggplot(aes(x = factor(Rt_case), y = Rt_kl)) +
  geom_boxplot(aes(col = method)) + 
  #facet_wrap(vars(prob.pars.dist, prob.pars.Rt_case)) + 
  scale_colour_manual(values = cbPalette) + 
  #coord_cartesian(ylim = c(0, 2.5)) + # excluding outliers (>3s) of EpiEstim
  scale_y_sqrt()+
  labs(y = "Averaged KL for negative Binomial incidences", x="Rt cases") +
  labs(color="Methods") +
  theme_bw()

#plot_ratio <- ggpubr::ggarrange(fig_logratio, fig_logratio_long, 
#                  fig_kl, fig_kl_long, nrow=2, nrol=2, common.legend=TRUE,
#                  legend="bottom")

ggsave(here::here("fig/runtime.png"), runtime_fig)
ggsave(here::here("fig/kl_pois.png"), fig_kl_pois, width = 5.63, height = 3.78)
ggsave(here::here("fig/kl_nb.png"), fig_kl_nb, width = 5.63, height = 3.78)


fig_kl <- ggpubr::ggarrange(fig_kl_pois, fig_kl_nb, ncol=2, nrow=1, 
                            common.legend = TRUE, legend = "bottom",
                            font.label = list(size = 14))
ggsave(here::here("fig/kl.png"), fig_kl, width = 7.41, height = 5.76)

