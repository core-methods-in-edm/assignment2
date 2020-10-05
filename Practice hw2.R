library(tidyverse)
library(tidyr)
library(dplyr)

D1 <- read.csv("video-data.csv", header = TRUE)
D1

D3 <- D1 %>% 
  summarise(mean_key = mean(key.points), med = median(key.points)) 

mode(D3$key.points)
plot(D3$year, D3$mean_key, type = "l", lty = "dashed") #linetype 

D4 <- filter(D1, stid == 4|stid == 20| stid == 22)
D4

D5 <- D1[,c(2,5,6,7)]
#Draw a matrix of plots for every combination of variables
pairs(D5)
