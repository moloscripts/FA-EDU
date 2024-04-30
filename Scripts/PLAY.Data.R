# References ####
# https://www.edc.org/playful-learning-across-years-20

# Libraries and Scripts ####
library(readxl)
library(tidyverse)
library(lubridate)
library(pander)
library(psych)
library(flextable)
library(cowplot)
library(janitor)
library(summarytools)
library(splitstackshape)

# library(reshape2)
#library(questionr)


# Street Child Color Harmonies ####


# Global Variables ####

# Function for autofit flextable object to docx document.
FitFlextableToPage <- function(ft, pgwidth = 6){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}


# Set border color of flexObject
set_flextable_defaults(
  border.color = "#8e8e8e"
)

# Colors to use in Data Visualisation
qlp1 <-"#FAA537"
qlp2 <- "#37B5FA"
qlp3 <-"#FA6137"
qlp4 <- "#5B8BA5"
qlp5 <- "#C25452"

# Darker shades
ds1 <- "#CF9850"
ds2 <- "#A5855B"
ds3 <- "#A97565"
ds4 <- "#AD8A67"
ds5 <- "#514438"

# Gray Shades
gs1 <- "#DFE0DF"
gs2 <- "#B8A99A"



# Data ####
CO.Scoring.Sheet <- read_xlsx("Data/Primary classroom Observation Scoring Sheet  (created 2024-04-02) 2024-04-02.xlsx") %>%
  filter(select_type_of_assessment %in% c("pilot_data_assessment", "irr")) %>%
  separate(started_time, c("Year", "Month", "Day"), "-") %>%
  filter(Year == "2024")

# replace subject other with art and craft
CO.Scoring.Sheet$lesson_subject[CO.Scoring.Sheet$lesson_subject == "other"] <- "art and craft"



Classroom.Observation.Inventory <- read_xlsx("Data/Primary Classroom Observation Inventory (C-O4) (created 2024-03-30) 2024-03-30.xlsx") %>%
  filter(select_type_of_assessment %in% c("pilot_data_assessment", "irr")) %>%
  separate(started_time, c("Year", "Month", "Day"), "-") %>%
  filter(Year == "2024")

Student.Interview <- read_xlsx("Data/Primary Student Interview (S-I1) (created 2024-04-02) 2024-04-02.xlsx") %>%
  filter(select_the_type_of_assessment %in% c("pilot_data_assessment", "irr")) %>%
  separate(started_time, c("Year", "Month", "Day"), "-") %>%
  filter(Year == "2024")

Teacher.Interview <- read_xlsx("Data/Primary Teacher Intervew (T-i1) (created 2024-04-02) 2024-04-02.xlsx") %>%
  filter(select_type_of_asessment %in% c("pilot_data_assessment", "irr")) %>%
  separate(started_time, c("Year", "Month", "Day"), "-") %>%
  filter(Year == "2024")

Learning.Centre.Mapping <- read_xlsx("Data/Learning Centre Mapping Tool (created 2024-04-02) 2024-04-02.xlsx") %>%
  separate(started_time, c("Year", "Month", "Day"), "-") %>%
  filter(Year == "2024")




