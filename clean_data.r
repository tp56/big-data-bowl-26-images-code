library(tidyverse)
library(dplyr)
library(plotly)

supplementary_data <- read.csv("data/114239_nfl_competition_files_published_analytics_final/supplementary_data.csv")
week1_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w01.csv")
week1_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w01.csv")

week2_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w02.csv")
week2_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w02.csv")

week3_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w03.csv")
week3_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w03.csv")

week4_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w04.csv")
week4_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w04.csv")

week5_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w05.csv")
week5_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w05.csv")

week6_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w06.csv")
week6_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w06.csv")

week7_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w07.csv")
week7_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w07.csv")

week8_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w08.csv")
week8_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w08.csv")

week9_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w09.csv")
week9_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w09.csv")

week10_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w10.csv")
week10_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w10.csv")

week11_input <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w11.csv")
week11_output <- read.csv("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w11.csv")