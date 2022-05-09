library(ggplot2)
library(stringi)
set.seed(123)
df <- data.frame(id = c(1:1000),
                 name = stri_rand_strings(n = 1000, length = 7, pattern = "[A-Za-z0-9]"),
                 class = sample(x = c("A", "B", "C", "D"), size = 1000, replace = TRUE, prob = c(0.25,0.25,0.25,0.25)),                         
                 gender = sample(x = c("female","male"), size = 1000, replace = TRUE, prob = c(0.5,0.5)),
                 district = sample(x = c(3:8), size = 1000, replace = TRUE, prob = c(0.2,0.1,0.25,0.15,0.2,0.1)),
                 campus = sample(x = c("shuttle bus","bicycle","car"), size = 1000, replace = TRUE, prob = c(0.55,0.2,0.25)),
                 math = round(rnorm(n=1000,mean=7.2,sd=0.6),1),
                 english = round(rnorm(n=1000,mean=8.1,sd=0.5),1)
                 )
View(df)
mygraph <- 
