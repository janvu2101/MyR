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