library(tidyverse)
library(infer)

#Data query source:
#https://console.cloud.google.com/bigquery?sq=746741777792:47d2aef6fa844b12a73c3ec79053643c 

#read in data
df <- read_csv('~/Downloads/bh_user_level_data.csv')
df <- df %>% mutate(
  enter_cc_categorical = if_else(enter_cc == 0, 'no', 'yes'),
  refunded_categorical = if_else(refunded == 0, 'no', 'yes'),
)


#clean up data
df %>% 
  group_by(experiment_group) %>% 
  summarise(
    n = n(), 
    refunded = sum(refunded),
    entered_credit_card = sum(enter_cc), 
    entry_rate = mean(enter_cc), 
    avg_value = mean(balance_dollar)
  )

# A tibble: 2 × 6
#   experiment_group    n     refunded   entered_credit_card  entry_rate  avg_value
#    <chr>             <int>    <dbl>               <dbl>       <dbl>     <dbl>
#   1 NO               164457     2265               17000      0.103      48.3
#   2 YES              164530      755               17241      0.105      57.3


#1. Cancellation rate test
# 1.1 using prop-test on experiment effect on cancellation (using cancellation as categorical var)
prop.test(x = c(755,2265), n = c(164530, 164457))

# 1.2 using t-test on experiment effect on cancellation (aymptotically same as prop test)
t_test(
  df, 
  refunded ~experiment_group,
  order = c("YES", "NO")
)


#2. Dollar charged effect
# t-test on experiment effect on dollar charged
t_test(
  df, 
  balance_dollar ~experiment_group,
  order = c("YES", "NO")
)


#3. prop test on experiment effect on entering cc 
prop.test(x = c(17241, 17000), n = c(164530, 164457))


