library(readr)
library(tidyverse)
library(dplyr)

#import weekly pitch-by-pitch data of 2021 MLB season from Baseball Savant and bind rows
setwd("~/Baseball/Datasets/Pitches2019")
week1_19 <- read.csv("2019week1.csv")
week2_19 <- read.csv("2019week2.csv")
week3_19 <- read.csv("2019week3.csv")
week4_19 <- read.csv("2019week4.csv")
week5_19 <- read.csv("2019week5.csv")
week6_19 <- read.csv("2019week6.csv")
week7_19 <- read.csv("2019week7.csv")
week8_19 <- read.csv("2019week8.csv")
week9_19 <- read.csv("2019week9.csv")
week10_19 <- read.csv("2019week10.csv")
week11_19 <- read.csv("2019week11.csv")
week12_19 <- read.csv("2019week12.csv")
week13_19 <- read.csv("2019week13.csv")
week14_19 <- read.csv("2019week14.csv")
week15_19 <- read.csv("2019week15.csv")
week16_19 <- read.csv("2019week16.csv")
week17_19 <- read.csv("2019week17.csv")
week18_19 <- read.csv("2019week18.csv")
week19_19 <- read.csv("2019week19.csv")
week20_19 <- read.csv("2019week20.csv")
week21_19 <- read.csv("2019week21.csv")
week22_19 <- read.csv("2019week22.csv")
week23_19 <- read.csv("2019week23.csv")
week24_19 <- read.csv("2019week24.csv")
week25_19 <- read.csv("2019week25.csv")
week26_19 <- read.csv("2019week26.csv")
week27_19 <- read.csv("2019week27.csv")

setwd("~/Baseball/Datasets/Pitches2020")
week1_20 <- read.csv("2020week1.csv")
week2_20 <- read.csv("2020week2.csv")
week3_20 <- read.csv("2020week3.csv")
week4_20 <- read.csv("2020week4.csv")
week5_20 <- read.csv("2020week5.csv")
week6_20 <- read.csv("2020week6.csv")
week7_20 <- read.csv("2020week7.csv")
week8_20 <- read.csv("2020week8.csv")
week9_20 <- read.csv("2020week9.csv")

setwd("~/Baseball/Datasets/Pitches2021")
week1_21 <- read.csv("2021week1.csv")
week2_21 <- read.csv("2021week2.csv")
week3_21 <- read.csv("2021week3.csv")
week4_21 <- read.csv("2021week4.csv")
week5_21 <- read.csv("2021week5.csv")
week6_21 <- read.csv("2021week6.csv")
week7_21 <- read.csv("2021week7.csv")
week8_21 <- read.csv("2021week8.csv")
week9_21 <- read.csv("2021week9.csv")
week10_21 <- read.csv("2021week10.csv")
week11_21 <- read.csv("2021week11.csv")
week12_21 <- read.csv("2021week12.csv")
week13_21 <- read.csv("2021week13.csv")
week14_21 <- read.csv("2021week14.csv")
week15_21 <- read.csv("2021week15.csv")
week16_21 <- read.csv("2021week16.csv")
week17_21 <- read.csv("2021week17.csv")
week18_21 <- read.csv("2021week18.csv")
week19_21 <- read.csv("2021week19.csv")
week20_21 <- read.csv("2021week20.csv")
week21_21 <- read.csv("2021week21.csv")
week22_21 <- read.csv("2021week22.csv")
week23_21 <- read.csv("2021week23.csv")
week24_21 <- read.csv("2021week24.csv")
week25_21 <- read.csv("2021week25.csv")
week26_21 <- read.csv("2021week26.csv")


df2019 <- bind_rows(week1_19,week2_19,week3_19,week4_19,week5_19,week6_19,week7_19,week8_19,week9_19,week10_19,week11_19,week12_19,week13_19,week14_19,week15_19,week16_19,week17_19,week18_19,week19_19,week20_19,week21_19,week22_19,week23_19,week24_19,week25_19,week26_19,week27_19)
df2020 <- bind_rows(week1_20,week2_20,week3_20,week4_20,week5_20,week6_20,week7_20,week8_20,week9_20)
df2021 <- bind_rows(week1_21,week2_21,week3_21,week4_21,week5_21,week6_21,week7_21,week8_21,week9_21,week10_21,week11_21,week12_21,week13_21,week14_21,week15_21,week16_21,week17_21,week18_21,week19_21,week20_21,week21_21,week22_21,week23_21,week24_21,week25_21,week26_21)

df <- bind_rows(df2019, df2020, df2021)

View(df)

#filtering for Joey Gallo's data to see what % of his HR's came on outside pitches
gallo <- df %>% 
  filter(player_name=="Gallo, Joey") 

gallo_hrtotal <- gallo %>% nrow(gallo %>% filter(events=="home_run"))

gallo_hroutside <- nrow(gallo %>%
  filter(events=="home_run" & zone %in% c(1,4,7,11,14,17,21,24,27,31,34,37)))

#gallo's result
gallo_hroutside/gallo_hrtotal*100

#now generalize it, and check all lefties in 2021 who had 20+ homers

res <- df %>%
  filter(stand=="L",
         events=="home_run") %>%
  group_by(player_name) %>%
  dplyr::summarize(hr_total = n(),
                   hr_outside = sum(zone %in% c(7,17)),
                   hroutside_percentage = hr_outside / hr_total * 100) %>%
  filter(hr_total >= 30) %>%
  arrange(desc(hroutside_percentage)) %>%
  select(player_name, hroutside_percentage, hr_total)

print(res,n=70)

