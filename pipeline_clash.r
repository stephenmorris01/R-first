# 1 load required packages
library(tidyverse)

# 2 read data into a data frame
df <- read.csv("C:\users\steph\OneDrive\Documents\R\Boot camp files\clashtroops.csv", header = TRUE)


# 3 clean data to focus on DPS
df <- filter(df, Type = "Normal")
df <- filter(df, Level >= 5)

# 4 visualize with a histogram
ggplot(data=df, aes(df$H)) + geom_histogram