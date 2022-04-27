library(dplyr)
library(stringi)
library(tidyr)
library(rio)
setwd("~/Documents/R/Self-learn-R")
module2 <- rio::import("card_krueger_public.dta")
var_name <- rio::import("variable_names.csv")
View(var_name)
names(module2) <- var_name$variable
str(module2)
set.seed(124)
simul_df <- data.frame(id = c(1:20),
                       name = stri_rand_strings(20, len = 7, pattern = '[A-zA-z0-9]'),
                       gender = sample(c('female','male'), size = 20, replace = TRUE, prob = c(0.5,0.5)),
                       class = sample(c('A','B','C'), size = 20, replace = TRUE, prob = c(0.3,0.3,0.4)),
                       campus = sample(c('yes','no'), size = 20, replace = TRUE, prob = c(0.25, 0.75)),
                       grade = sample(round(rnorm(20, mean = 7.4, sd = 0.7), 1))
                       )
table(simul_df$gender) %>%
  prop.table()
table(simul_df$class) %>%
  prop.table()
simul_df$campus %>% table()
simul_df$grade %>% hist()
class(simul_df$gender)
levels(simul_df$gender)
sapply(simul_df, class)

View(simul_df)
str(simul_df)

