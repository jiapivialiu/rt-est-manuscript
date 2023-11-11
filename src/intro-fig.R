library(rtestim)
cancovid <- cancovid[-1:-50, ] # drop the first 50 days
cbPalette <- c("#999999", "#CC79A7", "#0072B2", "#56B4E9")

cancovid$date[1]
n <- nrow(cancovid)
cancovid$date[n]

# plot Rt fit
cv_mod <- cv_estimate_rt(x = cancovid$date, 
                         cancovid$incident_cases,
                         korder = 1, nfold = 3, maxiter=1e7L, nsol = 50)
mod <- cv_mod$full_fit$Rt[ ,which.min(cv_mod$cv_scores)]
rt_ci_covid <- confband(cv_mod_covid, "lambda.min") # get 95% confidence band
fig_rt_fit <- rt_ci_covid %>%
  ggplot(aes(x = cancovid$date)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), 
              fill = "gray", alpha = 0.5) +
  geom_line(aes(y = Rt), col="#0072B2", lwd=1) +
  scale_x_date(date_breaks = "8 months", date_labels = "%Y-%m-%d") +
  geom_hline(yintercept = 1, linetype = "dotted") + 
  labs(x = "Date", y = "Piecewise linear Rt with 95% CIs") + 
  theme_bw()

# plot incidence fit
incidence_fit <- cv_mod$full_fit$weighted_past_counts * rt_ci_covid$Rt
inc_dat <- data.table(cancovid, incidence_fit = incidence_fit)
fig_inc_fit <- inc_dat %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = incident_cases/10000)) + 
  geom_line(aes(y = incidence_fit/10000), col="#D55E00", lwd=1) +
  scale_x_date(date_breaks = "8 months", date_labels = "%Y-%m-%d") +
  labs(x = "Date", y = "Fitted incidences (per 10,000)") + 
  theme_bw()

intro_fig <- ggarrange(fig_rt_fit, fig_inc_fit, ncol=1, 
                         common.legend = TRUE, legend = "bottom")
intro_fig

ggsave(here::here("fig/intro-fig.png"), intro_fig, width = 8, height = 5)
