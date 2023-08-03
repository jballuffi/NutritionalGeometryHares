library(ggplot2)

set.seed(10)
n <- 250
x <- runif(n,0,5)
y_model <- 3*x/(1+2*x)
y_obs <- rnorm(n,y_model,0.1)
data_plot <- qplot(x, y_obs) +
  geom_line(aes(y=y_model)) +
  theme_bw()
data_plot


library(mgcv)

gam_model <- gam(y_obs ~ s(x))
summary(gam_model)
data_plot <- data_plot +
  geom_line(colour = "blue", size = 1.2, aes(y = fitted(gam_model)))
data_plot
plot(gam_model)

linear_model <- gam(y_obs ~ x) # fit a regular linear model using gam()
nested_gam_model <- gam(y_obs ~ s(x))
anova(linear_model, nested_gam_model, test = "Chisq")


gam_data <-  gamSim(eg = 5)
# Additive model + factor
head(gam_data)

ggplot(gam_data)+
  geom_point(aes(y = y, x = x0))


basic_model <- gam(y ~ x0 + s(x1), data = gam_data)
basic_summary <- summary(basic_model)
basic_summary$p.table
#             Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) 8.550030  0.3655849 23.387258 1.717989e-76
# x02         2.418682  0.5165515  4.682364 3.908046e-06
# x03         4.486193  0.5156501  8.700072 9.124666e-17
# x04         6.528518  0.5204234 12.544629 1.322632e-30
basic_summary$s.table


anova(basic_model, two_term_model, two_smooth_model, test = "Chisq")