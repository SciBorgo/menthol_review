

# Calculate numbers for intro
# 2019 review data

d <- data.frame(study = c('2019_external', '2019_internal'),
                estimates = c(7,6),
                pooled_effect = c(0.29,0.4),
                lower = c(-0.34,0.04),
                upper = c(0.91,0.76),
                tau_square = c(0.49, 0.00))

k_correction <- 2
alpha <- 0.05 # should only by 0.05 unless changes to CI calc can be made
z_95 = qnorm(p = 1 - alpha/2) # Z-score for calculation of standard error
t_dist_fun <- function(k,alpha){qt(p = 1 - {{alpha}}/2, df={{k}}-k_correction)} # t distribution needed for calculation of prediction intervals

df <-
  d %>% mutate(
  se = ((upper-lower)/(2*z_95)),
  se_square = se^2,
  estimates = as.integer(estimates),
  pooled_effect = as.numeric(pooled_effect),
  pi_lower_calc = pooled_effect-t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square),
  pi_upper_calc = pooled_effect+t_dist_fun(k = estimates, alpha = alpha)*sqrt(se_square+tau_square))

df
