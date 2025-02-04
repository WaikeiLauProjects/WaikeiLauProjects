
library(lubridate)
library(dplyr)

Dmd_NSW <- read.csv("C:/Users/user_001/Downloads/project-main/project-main/data/totaldemand_nsw.csv")
Dmd_NSW$year <- year(Dmd_NSW$DATETIME)
Dmd_2010_2017 <- Dmd_NSW %>%
  filter(year == 2010:2017)
Dmd_2018_2022 <- Dmd_NSW %>%
  filter(year == 2018:2022)

write.csv(Dmd_2010_2017, "C:/Users/user_001/Downloads/project-main/project-main/data/demand 2010-2017.csv")
write.csv(Dmd_2018_2022, "C:/Users/user_001/Downloads/project-main/project-main/data/demand 2018-2022.csv")

