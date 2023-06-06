#Mediation analysis

#Total effect: regress experiment treatment on the cancellation rate
total_effect <- lm(refunded ~ experiment_group, data = df)
summary(total_effect)

#Effect of IV on mediator: regress experiment treatment on to the mediator
iv_to_mediator_effect <- lm(enter_cc ~ experiment_group, data = df)
summary(iv_to_mediator_effect)

#Effect of mediator on outcome var, controlling for IV
mediator_to_dv <- lm(refunded ~ experiment_group + enter_cc, data = df)
summary(mediator_to_dv)


#Mediation analysis
library(mediation)
results <-  mediate(iv_to_mediator_effect, mediator_to_dv, treat='experiment_group', mediator='enter_cc', boot=T)
summary(results)



#Delta method
exp_agg_data <- tibble(
  group = c('test','control'), 
  user_size = c(164530, 164457),
  cancellation_rate_mean  = c(0.004588828785, 0.013772597092), 
  cancellation_rate_se = sqrt(cancellation_rate_mean * (1-cancellation_rate_mean)/user_size),
  enter_cc_mean = c(0.104789400109, 0.1033704859020),
  enter_cc_se = sqrt(enter_cc_mean * (1-enter_cc_mean)/user_size), 
  balance_charged_mean = c(57.257120282015, 48.25526429400),
  balance_charged_se = c(146.3827882149, 131.5835128047)
)

delta_method_ratio_variance <- function(test_mean, control_mean, test_se, control_se){
  ratio_variance <- (test_se ^ 2 +  (test_mean/control_mean)^2 * control_se^2)/ control_mean^2
  return(ratio_variance)
}


pct_delta_var <- delta_method_ratio_variance(
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'control'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'control']
  )


pct_delta <- exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'test']/exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'control']-1

c(pct_delta, pct_delta + 1.96*sqrt(pct_delta_var), pct_delta - 1.96*sqrt(pct_delta_var))



pct_relative_delta_difference <- function(test_mean, control_mean, test_se, control_se, test_sample_size, control_sample_size, confidence_level){
  pct_delta <- test_mean/control_mean -1
  pct_delta_var <- delta_method_ratio_variance2(test_mean, control_mean, test_se, control_se)
  t_stats <- qt(1-((1-0.95)/2), (test_sample_size + control_sample_size -2))
  upper_ci <- pct_delta + t_stats * sqrt(pct_delta_var)
  lower_ci <- pct_delta - t_stats * sqrt(pct_delta_var)
  
  stat_data <- c(confidence_level, 
                 paste0(round(pct_delta, 4)*100, '%'),
                 paste0(round(upper_ci, 4)*100, '%'), 
                 paste0(round(lower_ci, 4)*100, '%'), 
                 paste0(round(sqrt(pct_delta_var), 4)*100, '%'))
  names(stat_data) <- c('Confidence level', 
                        '% delta (relative)', 
                        '% delta upper limit', 
                        '% delta lower limit', 
                        '% delta se')
  return(stat_data)
}

#Test cancellation
pct_relative_delta_difference(
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'control'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'control'],
  exp_agg_data$user_size[exp_agg_data$group == 'test'],
  exp_agg_data$user_size[exp_agg_data$group == 'control'],
  '0.95'
)

#Test enter CC
pct_relative_delta_difference(
  exp_agg_data$enter_cc_mean[exp_agg_data$group == 'test'],
  exp_agg_data$enter_cc_mean[exp_agg_data$group == 'control'],
  exp_agg_data$enter_cc_se[exp_agg_data$group == 'test'],
  exp_agg_data$enter_cc_se[exp_agg_data$group == 'control'],
  exp_agg_data$user_size[exp_agg_data$group == 'test'],
  exp_agg_data$user_size[exp_agg_data$group == 'control'],
  '0.95'
)

#Test balance charged
pct_relative_delta_difference(
  exp_agg_data$balance_charged_mean[exp_agg_data$group == 'test'],
  exp_agg_data$balance_charged_mean[exp_agg_data$group == 'control'],
  exp_agg_data$balance_charged_se[exp_agg_data$group == 'test']/sqrt(exp_agg_data$user_size[exp_agg_data$group == 'test']),
  exp_agg_data$balance_charged_se[exp_agg_data$group == 'control']/sqrt(exp_agg_data$user_size[exp_agg_data$group == 'control']),
  exp_agg_data$user_size[exp_agg_data$group == 'test'],
  exp_agg_data$user_size[exp_agg_data$group == 'control'],
  '0.95'
)

delta_method_ratio_variance2 <- function(mean_test, mean_control, se_test, se_control){
  var_calc <- (se_test^2 * mean_control^2 + se_control^2 * mean_test^2)/mean_control^4
  return(var_calc)
}


delta_method_ratio_variance(
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'control'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'control']
)

delta_method_ratio_variance2(
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_mean[exp_agg_data$group == 'control'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'test'],
  exp_agg_data$cancellation_rate_se[exp_agg_data$group == 'control']
)

delta_method_ratio_variance2(
  exp_agg_data$balance_charged_mean[exp_agg_data$group == 'test'],
  exp_agg_data$balance_charged_mean[exp_agg_data$group == 'control'],
  exp_agg_data$balance_charged_se[exp_agg_data$group == 'test'],
  exp_agg_data$balance_charged_se[exp_agg_data$group == 'control']
)


library(lubridate)
ymd('2023-12-31')-(365/2)
