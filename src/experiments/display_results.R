Rt_result <- readRDS("src/experiments/rt_exp7_results_all.RDS")

library(ggplot2)
library(forcats)
library(dplyr)
cbPalette <- c("#F0E442", "#E69F00", "#009E73", "#CC79A7", "#0072B2", "#56B4E9", 
               "#D55E00")
#cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#56B4E9", "#999999", 
#               "#F0E442", "#D55E00", "#CC79A7")
Rt_result$Rt_case <- as.factor(Rt_result$Rt_case)
Rt_result$dist <- as.factor(Rt_result$dist)
Rt_result$method <- as.factor(Rt_result$method)

## CV is time consuming, especially for many lambdas
runtime_fig <- Rt_result %>%
  #filter(runtime <= 5*1e6) %>% #Rt k=3 run slowly >1000s sometimes
  group_by(dist, Rt_case, method) %>%
  ggplot(aes(x = Rt_case, y =runtime/1e6)) +
  geom_boxplot(aes(col = method)) + 
  facet_wrap(vars(dist)) + 
  scale_colour_manual(values = cbPalette) + 
  #scale_y_log10() + 
  labs(y = "Running time in seconds", x = "Rt scenarios") + 
  labs(color = "Methods") +
  theme_bw()
runtime_fig

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
  #filter(Rt_case != 2) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  filter(dist == "poisson") %>%
  select(Rt_case, method, Rt_kl) %>% 
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 1" = '1',
                             "Scenario 2" = "2",
                             "Scenario 3" = "3",
                             "Scenario 4" = "4")) %>% 
  mutate(method = fct_recode(method, "EpiEstim" = "EpiEstim(week)",
                             "RtEstim (k=0)" = "RtEstim(k=0)", 
                             "RtEstim (k=1)" = "RtEstim(k=1)", 
                             "RtEstim (k=3)" = "RtEstim(k=3)")) %>% 
  mutate(method = fct_relevel(method, "EpiEstim", "EpiLPS", 
                              "RtEstim (k=0)", "RtEstim (k=1)", 
                              "RtEstim (k=3)")) %>% 
  group_by(Rt_case, method) %>%
  ggplot(aes(x = Rt_case, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  #facet_grid(~Rt_case, scales="free") + 
  scale_colour_manual(values = cbPalette[-1]) + 
  coord_cartesian(ylim = c(1e-5, 0.02)) + #remove a outlier of EpiLPS >.5*0.001
  labs(y = "Averaged KL for Poisson incidences", x = "Rt Scenarios") +
  labs(color="Methods") +
  theme_bw()
fig_kl_pois

fig_kl_nb <- Rt_result %>%
  filter(dist == "NB") %>%
  #filter(Rt_case != 2) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 1" = '1',
                              "Scenario 2" = "2",
                              "Scenario 3" = "3",
                              "Scenario 4" = "4")) %>% 
  mutate(method = fct_recode(method, "EpiEstim" = "EpiEstim(week)",
                             "RtEstim (k=0)" = "RtEstim(k=0)", 
                             "RtEstim (k=1)" = "RtEstim(k=1)", 
                             "RtEstim (k=3)" = "RtEstim(k=3)")) %>% 
  mutate(method = fct_relevel(method, "EpiEstim", "EpiLPS", 
                              "RtEstim (k=0)", "RtEstim (k=1)", 
                              "RtEstim (k=3)")) %>% 
  group_by(Rt_case, method) %>%
  #filter(Rt_kl <= 300) %>% # remove a few case2 EpiLPS (>300)
  ggplot(aes(x = Rt_case, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  #geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey") +
  scale_colour_manual(values = cbPalette[-1]) + 
  coord_cartesian(ylim = c(0, .06)) + # remove an outlier of Rt (>.5*0.001)
  labs(y = "Averaged KL ratio for NB incidences", x="Rt Scenarios") +
  labs(color="Methods") +
  theme_bw()
fig_kl_nb

ggsave(here::here("fig/runtime.png"), runtime_fig)
ggsave(here::here("fig/kl_pois.png"), fig_kl_pois, width = 5.63, height = 3.78)
ggsave(here::here("fig/kl_nb.png"), fig_kl_nb, width = 5.63, height = 3.78)

fig_kl <- ggpubr::ggarrange(fig_kl_pois, fig_kl_nb, ncol=2, nrow=1, 
                            common.legend = TRUE, legend = "bottom",
                            font.label = list(size = 14))
ggsave(here::here("fig/kl.png"), fig_kl, width = 6.57, height = 4.26)
  
## separate for each scenario ----
fig_kl_pois2 <- Rt_result %>%
  select(Rt_case, dist, method, Rt_kl) %>% 
  filter(Rt_case == 2) %>%
  filter(dist == "poisson") %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 2" = '2')) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  group_by(method) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  facet_grid(~Rt_case) +
  scale_colour_manual(values = cbPalette[c(2,3,5,6)]) + 
  coord_cartesian(ylim = c(0, 0.95*.001)) + #remove a outlier of EpiLPS >.5*0.001
  labs(y = "Averaged KL for Poisson incidences", x = "") +
  labs(color="Methods") +
  theme_bw() +
  theme(legend.position = "none")

fig_kl_nb2 <- Rt_result %>%
  select(Rt_case, dist, method, Rt_kl) %>% 
  filter(Rt_case == 2) %>%
  filter(dist == "NB") %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 2" = '2')) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  group_by(method) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  facet_grid(~Rt_case) +
  scale_colour_manual(values = cbPalette[c(2,3,5,6)]) + 
  coord_cartesian(ylim = c(0, 25*.001)) + #remove a outlier of EpiLPS >.5*0.001
  labs(y = "Averaged KL for NB incidences", x = "") +
  labs(color="Methods") +
  theme_bw() +
  theme(legend.position = "none")

fig_kl2 <- ggpubr::ggarrange(fig_kl_pois2, fig_kl_nb2, ncol=2, nrow=1, 
                            #common.legend = TRUE, legend = "bottom",
                            font.label = list(size = 14))
fig_kl2
ggsave(here::here("fig/kl2.png"), fig_kl2, width = 7.72, height = 4.16)


fig_kl_pois1 <- Rt_result %>%
  select(Rt_case, dist, method, Rt_kl) %>% 
  filter(Rt_case == 1) %>%
  filter(dist == "poisson") %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 1" = '1')) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  group_by(method) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  facet_grid(~Rt_case) +
  scale_colour_manual(values = cbPalette[c(2,3,4)]) + 
  coord_cartesian(ylim = c(0, .02)) + #remove a outlier of EpiLPS >.5*0.001
  labs(y = "Averaged KL for Poisson incidences", x = "") +
  labs(color="Methods") +
  theme_bw() +
  theme(legend.position = "none")
fig_kl_pois1

fig_kl_nb1 <- Rt_result %>%
  select(Rt_case, dist, method, Rt_kl) %>% 
  filter(Rt_case == 1) %>%
  filter(dist == "NB") %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 1" = '1')) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  group_by(method) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  facet_grid(~Rt_case) +
  scale_colour_manual(values = cbPalette[c(2,3,4)]) + 
  coord_cartesian(ylim = c(0, .06)) + #remove a outlier of EpiLPS >.5*0.001
  labs(y = "Averaged KL for NB incidences", x = "") +
  labs(color="Methods") +
  theme_bw() +
  theme(legend.position = "none")
fig_kl_nb1

fig_kl1 <- ggpubr::ggarrange(fig_kl_pois1, fig_kl_nb1, ncol=2, nrow=1, 
                             #common.legend = TRUE, legend = "bottom",
                             font.label = list(size = 14))
fig_kl1
ggsave(here::here("fig/kl1.png"), fig_kl1, width = 7.72, height = 4.16)


fig_kl_pois3 <- Rt_result %>%
  select(Rt_case, dist, method, Rt_kl) %>% 
  filter(Rt_case == 3) %>%
  filter(dist == "poisson") %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 3" = '3')) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  group_by(method) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  facet_grid(~Rt_case) +
  scale_colour_manual(values = cbPalette[c(2,3,5)]) + 
  coord_cartesian(ylim = c(0, .02)) + #remove a outlier of EpiLPS >.5*0.001
  labs(y = "Averaged KL for Poisson incidences", x = "") +
  labs(color="Methods") +
  theme_bw() +
  theme(legend.position = "none")
fig_kl_pois3

fig_kl_nb3 <- Rt_result %>%
  select(Rt_case, dist, method, Rt_kl) %>% 
  filter(Rt_case == 3) %>%
  filter(dist == "NB") %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 3" = '3')) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  group_by(method) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) +
  facet_grid(~Rt_case) +
  scale_colour_manual(values = cbPalette[c(2,3,5)]) + 
  coord_cartesian(ylim = c(0, .05)) + #remove a outlier of EpiLPS >.5*0.001
  labs(y = "Averaged KL for NB incidences", x = "") +
  labs(color="Methods") +
  theme_bw() +
  theme(legend.position = "none")
fig_kl_nb3

fig_kl3 <- ggpubr::ggarrange(fig_kl_pois3, fig_kl_nb3, ncol=2, nrow=1, 
                             #common.legend = TRUE, legend = "bottom",
                             font.label = list(size = 14))
fig_kl3
ggsave(here::here("fig/kl3.png"), fig_kl3, width = 7.72, height = 4.16)


# scenario 4
fig_kl_pois4 <- Rt_result %>%
  select(Rt_case, dist, method, Rt_kl) %>% 
  filter(Rt_case == 4) %>%
  filter(dist == "poisson") %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 4" = '4')) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  group_by(method) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  facet_grid(~Rt_case) +
  scale_colour_manual(values = cbPalette[c(2,3,6)]) + 
  coord_cartesian(ylim = c(0, .009)) + #remove a outlier of EpiLPS >.5*0.001
  labs(x="",y = "Averaged KL for Poisson incidences") +
  theme_bw() +
  theme(legend.position = "none")
fig_kl_pois4

fig_kl_nb4 <- Rt_result %>%
  select(Rt_case, dist, method, Rt_kl) %>% 
  filter(Rt_case == 4) %>%
  filter(dist == "NB") %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 4" = '4')) %>%
  filter(method != "EpiEstim(month)") %>% # weekly sliding only
  group_by(method) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  facet_grid(~Rt_case) +
  scale_colour_manual(values = cbPalette[c(2,3,6)]) + 
  coord_cartesian(ylim = c(0, .04)) + #remove a outlier of EpiLPS >.5*0.001
  labs(x="", y = "Averaged KL for NB incidences") +
  theme_bw() +
  theme(legend.position = "none")
fig_kl_nb4

fig_kl4 <- ggpubr::ggarrange(fig_kl_pois4, fig_kl_nb4, ncol=2, nrow=1, 
                             #common.legend = TRUE, legend = "bottom",
                             font.label = list(size = 14))
fig_kl4
ggsave(here::here("fig/kl4.png"), fig_kl4, width = 7.72, height = 4.16)

ggpubr::ggarrange(fig_kl_pois1, fig_kl_nb1, 
                  fig_kl_pois2, fig_kl_nb2, 
                  fig_kl_pois3, fig_kl_nb3, 
                  fig_kl_pois4, fig_kl_nb4, 
                  ncol=2, nrow=4, 
                  #common.legend = TRUE, legend = "bottom",
                  font.label = list(size = 14))

## remove outliers ----
Rt_result_no_outliers <- Rt_result |>
  filter(method != "EpiEstim(month)") |>
  select(Rt_case, dist, method, Rt_kl) |>
  group_by(Rt_case, dist, method) |>
  mutate(big_out = Rt_kl > fivenum(Rt_kl, na.rm = TRUE)[4] + 
           1.5 * IQR(Rt_kl, na.rm = TRUE)) |>
  filter(!big_out) |>
  select(!big_out)
Rt_result_no_outliers$Rt_case <- as.factor(Rt_result_no_outliers$Rt_case)
Rt_result_no_outliers$dist <- as.factor(Rt_result_no_outliers$dist)
Rt_result_no_outliers$method <- as.factor(Rt_result_no_outliers$method)

KL_fig_no_outlier <- Rt_result_no_outliers %>%
  mutate(Rt_case = fct_recode(Rt_case, "Scenario 1" = '1',
                              "Scenario 2" = '2',
                              "Scenario 3" = '3',
                              "Scenario 4" = '4')) %>%
  mutate(dist = fct_recode(dist, "Poisson" = "poisson",
                           "Negative Binomial" = "NB")) %>% 
  mutate(dist = fct_relevel(dist, "Poisson", "Negative Binomial")) %>% 
  mutate(method = fct_recode(method, 
                             "EpiEstim" = "EpiEstim(week)",
                             "RtEstim (k=0)" = "RtEstim(k=0)", 
                             "RtEstim (k=1)" = "RtEstim(k=1)", 
                             "RtEstim (k=3)" = "RtEstim(k=3)")) %>% 
  #mutate(Rt_kl = purrr::map_dbl(Rt_kl, ~ remove_outliers(.x))) %>%
  ggplot(aes(x=method, y = Rt_kl*300)) +
  geom_boxplot(aes(col = method)) + 
  facet_grid(dist ~ Rt_case, scales="free") +
  #scale_y_log10() + 
  scale_colour_manual(values = cbPalette[-1]) + 
  #coord_cartesian(ylim = c(0, .04)) + #remove a outlier of EpiLPS >.5*0.001
  labs(x="Method", y = "KL divergence") +
  theme_bw() + 
  theme(axis.text.x = element_blank(), legend.position="bottom") +
  guides(color = guide_legend(title = NULL))
KL_fig_no_outlier

ggsave(here::here("fig/KL_no_outlier.png"), KL_fig_no_outlier, 
       width = 7.34, height = 6.06)
