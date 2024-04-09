# References ####
# https://www.edc.org/playful-learning-across-years-20


# Set seed for creating reproducible results ####
set.seed(3335)

# Libraries and Scripts ####
library(readxl)
library(tidyverse)
library(lubridate)

# Street Child Color Harmonies
source("Scripts/Color.Harmonies.R")




# Data ####
C0.Scoring.Sheet <- read_xlsx("Data/Primary classroom Observation Scoring Sheet  (created 2024-04-02) 2024-04-02.xlsx") %>%
  filter(select_type_of_assessment %in% c("pilot_data_assessment", "irr"))


# Classroom.Observation.Inventory <- read_xlsx("Data/Primary Classroom Observation Inventory (C-O4) (created 2024-03-30) 2024-03-30.xlsx") %>%
#   filter(form.select_type_of_assessment %in% c("pilot_data_assessment", "irr"))
# 
# 
# Student.Interview <- read_xlsx("Data/Primary Student Interview (S-I1) (created 2024-04-02) 2024-04-02.xlsx") %>%
#   filter(form.question1.demographics.select_the_type_of_assessment %in% c("pilot_data_assessment", "irr"))
# 
# Teacher.Interview <- read_xlsx("Data/Primary Teacher Intervew (T-i1) (created 2024-04-02) 2024-04-02.xlsx") %>%
#   filter(form.set_of_questions.section_i.select_type_of_asessment %in% c("pilot_data_assessment", "irr"))
# 
# Learning.Centre.Mapping <- read_xlsx("Data/Learning Centre Mapping Tool (created 2024-04-02) 2024-04-02.xlsx")
# 


# Analysis ####

## Classroom Observation Scoring Sheet ####

# Constructs
# COX: Support from Exploration
# COA: Support for Agency
# COC: Support for personal and social connection 
# COE: Support for emotional climate

# Data descriptives
table(C0.Scoring.Sheet$form.demographics.community)

# Check the time taken for observation.



