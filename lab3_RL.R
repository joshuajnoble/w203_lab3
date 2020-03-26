install.packages("tidyr")
library(tidyr)
library(ggplot2)

mtcars %>% gather() %>% head()

ggplot(gather(deterrent_data[1:91,]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

ggplot(gather(mtcars), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')