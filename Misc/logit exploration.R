library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

logit <- function(x){
  exp(x) / (1 + exp(x))
}


data <- data.frame(x= -10:10) %>%
  mutate(y = logit(x)) %>%
  mutate(y1_5 = logit(x / 1.5)) %>%
  mutate(y2 = logit(x / 2)) %>%
  mutate(y2_5 = logit(x / 2.5)) %>%
  mutate(y3 = logit(x / 3)) %>%
  mutate(p1 = pnorm(x, 0, 3)) %>%
  gather(type, value, -x)




p <- ggplot(data, aes(x = x, y = value, color = type)) + geom_line() 
p %>%
  ggplotly()

