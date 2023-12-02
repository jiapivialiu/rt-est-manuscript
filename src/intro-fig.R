library(rtestim)
library(ggplot2)
library(data.table)
library(ggpubr)
cancovid <- cancovid[-1:-65, ] # drop the first 50 days
cbPalette <- c("#999999", "#CC79A7", "#0072B2", "#56B4E9")

cancovid$date[1]
n <- nrow(cancovid)
cancovid$date[n]

# plot Rt fit
cv_mod <- cv_estimate_rt(x = cancovid$date, cancovid$incident_cases,
                         korder = 2, nfold = 10, maxiter = 1e7L, 
                         nsol = 50,
                         error_measure = "deviance",
                         lambda_min_ratio=1e-6,
                         init = configure_rt_admm(tol=1e-6))
#plot(cv_mod)
rt_ci_covid <- confband(cv_mod, "lambda.1se") # get 95% confidence band
mod_best <- estimate_rt(x = cancovid$date, cancovid$incident_cases,
                   korder = 2, maxiter = 1e7L, #nsol = 100)
                   lambda = cv_mod$lambda.1se)
#plot(mod_best)
#which(abs(mod_best$alp) > mod_best$tolerance) 
# knots at 599, 619 for 5-fold CV for lambda.1se, 
# at 577 600 618 for lambda.min 
#find_knots(mod_best, mod_best$lambda)

#plot(rt_ci_covid)
fig_rt_fit <- rt_ci_covid %>%
  ggplot(aes(x = cancovid$date)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), 
              fill = "gray", alpha = 0.5) +
  geom_line(aes(y = Rt), col="#0072B2", lwd=1) +
  scale_x_date(date_breaks = "8 months", date_labels = "%Y-%m-%d") +
  geom_hline(yintercept = 1, linetype = "dotted") + 
  labs(x = "Date", y = "Piecewise quadratic Rt with 95% CIs") + 
  theme_bw()
fig_rt_fit

# plot incidence fit
incidence_fit <- mod_best$weighted_past_counts * rt_ci_covid$Rt
inc_dat <- data.table(cancovid, incidence_fit = incidence_fit)
fig_inc_fit <- inc_dat %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = incident_cases/10000)) + 
  geom_line(aes(y = incidence_fit/10000), col="#D55E00", lwd=1) +
  scale_x_date(date_breaks = "8 months", date_labels = "%Y-%m-%d") +
  labs(x = "Date", y = "Fitted incidence (per 10,000)") + 
  theme_bw()

intro_fig <- ggarrange(fig_rt_fit, fig_inc_fit, ncol=1, 
                         common.legend = TRUE, legend = "bottom")
intro_fig

ggsave(here::here("fig/intro-fig-new.png"), intro_fig, width = 7.34, height = 6.06)
