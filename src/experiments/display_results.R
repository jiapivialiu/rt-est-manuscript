
library(ggplot2)
library(forcats)
cbPalette <- c("#F0E442", "#E69F00", "#009E73", "#CC79A7", "#0072B2", "#56B4E9", 
               "#D55E00")
#cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#56B4E9", "#999999", 
#               "#F0E442", "#D55E00", "#CC79A7")
Rt_result$Rt_case <- as.factor(Rt_result$Rt_case)
Rt_result$dist <- as.factor(Rt_result$dist)

## CV is time consuming, especially for many lambdas
runtime_fig <- Rt_result %>%
  filter(runtime <= 5*1e6) %>% #Rt k=3 run slowly >1000s sometimes
  group_by(dist, Rt_case, method) %>%
  ggplot(aes(x = Rt_case, y =runtime/1e6)) +
  geom_boxplot(aes(col = method)) + 
  facet_wrap(vars(dist)) + 
  scale_colour_manual(values = cbPalette) + 
  labs(y = "Running time in seconds", x = "Rt scenarios") + 
  labs(color = "Methods") +
  theme_bw()

# see Supp
#fig_kl_pois_full <- Rt_result %>%
#  filter(method != "EpiEstim(month)") %>% # weekly sliding only
#  filter(dist == "poisson") %>%
#  select(Rt_case, method, Rt_kl, KL_base) %>% 
#  mutate(Rt_kl = Rt_kl / KL_base) %>%
#  mutate(method = fct_recode(method, "EpiEstim" = "EpiEstim(week)")) %>% 
#  mutate(method = fct_relevel(method, "EpiEstim", "EpiLPS", 
#                              "RtEstim(k=0)", "RtEstim(k=1)", 
#                              "RtEstim(k=3)")) %>% 
#  group_by(Rt_case, method) %>%
#  #filter(Rt_kl < 50) %>% # one extreme value for case2 EpiLPS (4694.044), and 3 case2 EpiEstim (107.1923 and 10-60)
#  ggplot(aes(x = factor(Rt_case), y = Rt_kl)) +
#  geom_boxplot(aes(col = method)) + 
#  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey") +
#  #annotate("text", x = 0, y = 1, label="1") + 
#  #facet_wrap(vars(prob.pars.dist, prob.pars.Rt_case)) + 
#  scale_y_log10(breaks = c(1,6,20,40,60,100,4000), 
#               labels = c("1","6","20","40","60","100","4000")) + 
#  scale_colour_manual(values = cbPalette[-1]) + 
#  #coord_cartesian(ylim = c(0, 7)) + 
#  labs(y = "Averaged KL ratio for Poisson incidences", x="Rt Scenarios") +
#  labs(color="Methods") +
#  theme_bw()

fig_kl_pois <- Rt_result %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  filter(dist == "poisson") %>%
  select(Rt_case, method, Rt_kl, KL_base) %>% 
  mutate(Rt_kl = Rt_kl / KL_base) %>%
  mutate(method = fct_recode(method, "EpiEstim" = "EpiEstim(week)")) %>% 
  mutate(method = fct_relevel(method, "EpiEstim", "EpiLPS", 
                              "RtEstim(k=0)", "RtEstim(k=1)", 
                              "RtEstim(k=3)")) %>% 
  group_by(Rt_case, method) %>%
  filter(Rt_kl < 10) %>% # one extreme value for case2 EpiLPS (4694.044), and 3 case2 EpiEstim (107.1923 and 10-60)
  ggplot(aes(x = Rt_case, y = Rt_kl)) +
  geom_boxplot(aes(col = method)) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey") +
  scale_y_sqrt(breaks = c(1,2,4,6,8), 
                labels = c("1","2","4","6","8")) + 
  scale_colour_manual(values = cbPalette[-1]) + 
  #coord_cartesian(ylim = c(0, 7)) + 
  labs(y = "Averaged KL ratio for Poisson incidences", x="Rt Scenarios") +
  labs(color="Methods") +
  theme_bw()

#fig_kl_nb_full <- Rt_result %>%
#  filter(dist == "NB") %>%
#  filter(method != "EpiEstim(month)") %>% # weekly sliding only
#  mutate(method = fct_recode(method, "EpiEstim" = "EpiEstim(week)")) %>% 
#  mutate(Rt_kl = Rt_kl / KL_base) %>%
#  group_by(Rt_case, method) %>%
#  #filter(Rt_kl <= 300) %>% # remove a few case2 EpiLPS (>300)
#  ggplot(aes(x = factor(Rt_case), y = Rt_kl)) +
#  geom_boxplot(aes(col = method)) + 
#  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey") +
#  #facet_wrap(vars(prob.pars.dist, prob.pars.Rt_case)) + 
#  scale_y_log10(breaks = c(1,6,50,200,4000), 
#               labels = c("1","6","50","200","4000"))+
#  scale_colour_manual(values = cbPalette[-1]) + 
#  #coord_cartesian(ylim = c(0, 2.5)) + 
#  labs(y = "Averaged KL ratio for negative Binomial incidences", x="Rt Scenarios") +
#  labs(color="Methods") +
#  theme_bw()

fig_kl_nb <- Rt_result %>%
  filter(dist == "NB") %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  mutate(method = fct_recode(method, "EpiEstim" = "EpiEstim(week)")) %>% 
  mutate(Rt_kl = Rt_kl / KL_base) %>%
  group_by(Rt_case, method) %>%
  filter(Rt_kl <= 300) %>% # remove a few case2 EpiLPS (>300)
  ggplot(aes(x = factor(Rt_case), y = Rt_kl)) +
  geom_boxplot(aes(col = method)) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey") +
  #facet_wrap(vars(prob.pars.dist, prob.pars.Rt_case)) + 
  scale_y_sqrt(breaks = c(1,6,50,100,200,4000), 
                labels = c("1","6","50","100","200","4000"))+
  scale_colour_manual(values = cbPalette[-1]) + 
  #coord_cartesian(ylim = c(0, 2.5)) + 
  labs(y = "Averaged KL ratio for NB incidences", x="Rt Scenarios") +
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
ggsave(here::here("fig/kl.png"), fig_kl, width = 6.57, height = 4.26)


