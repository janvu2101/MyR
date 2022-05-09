#https://r4ds.had.co.nz/data-visualisation.html
library(tidyverse)
library(ggplot2)
View(mpg) #mpg is built-in dataset in tidyverse library
#SCATTER PLOT with geom_point()
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y=hwy))
#color by "class"
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y=hwy, color = class))
#size by "class"
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y=hwy, size = class))
#alpha (transparency) by "class"
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y=hwy, alpha = class))
#shape by "class" - default 6 shapes at a time, the 7th goes unplotted
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y=hwy, shape = class), color = "darkgreen")
#save plot object: a<- ggplot(). Then we can save a as a png file or whatever we would like

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
graph <- ggplot(data = df) +
  geom_point(mapping = aes(x = math, y = english))
graph <- ggplot(data = df) +
  geom_point(mapping = aes(x = math, y = english, color = gender, shape = gender)) +
  scale_color_manual(values = c("darkgreen","orange")) +
  scale_shape_manual(values = c(3,4))
graph
#color picker here: https://htmlcolorcodes.com/
#shape picker here: http://www.sthda.com/english/wiki/ggplot2-point-shapes

#If we have continuous scale, we can use gradient colors

grad_graph <- ggplot(data = df) +
  geom_point(mapping = aes(x = math, y = english, color = math)) +
  scale_color_gradient(low = "darkgreen", high = "orange", name = "colored math")
grad_graph

#Axis: xlim() + ylim()
axis <- ggplot(df) +
  geom_point(aes(x = math, y = english, color = gender, shape = gender)) +
  scale_color_manual(values = c("darkgreen","orange")) +
  xlim(6,10) + ylim(5,10)
axis #we get warning message because some data points are out of bounds, hence, not plotted on the graph

#Title: labs()
title <- ggplot(df) +
  geom_point(aes(x = math, y = english, color = gender, shape = gender)) +
  scale_color_manual(values = c("darkgreen","orange")) +
  labs(x = "math_grade",
       y = "english_grade",
       color = "classified by gender",
       shape = "classified by gender")
title

#Add straight line to the graph: geom_vline() or geom_hline()

#Add ablines and regression lines



























