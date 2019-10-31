############################
### Prepare BES data set ###
############################

library(magrittr)
library(tidyr)
library(dplyr)
library(foreign)

# Read in data

data <- read.dta("./bes_f2f_2017_v1.3.dta")

# Keep only six variables

data %<>%
  dplyr::select(Age, y11, y09, e01, y48_1, education)

# Recode variables

## Age

data %<>% mutate(Age = as.numeric(Age)) %>%
  mutate(Age = ifelse(Age == -2, NA, Age))

## Gender

data %<>% rename(Gender = y09)
data %<>% mutate(Gender = case_when(Gender == "Male" ~ "0",
                                    Gender == "Female" ~ "1",
                                    Gender == "Not states" ~ "NA",
                                    TRUE ~ as.character(Gender))) %>%
  mutate(Gender = as.numeric(Gender))

## Education

data %<>% rename(Education = education)
data %<>% mutate(Education = as.character(Education)) %>%
  mutate(Education = case_when(Education == "Univ/poly diploma" |
                                 Education == "Teaching qualification" |
                                 Education == "Nursing qualification" |
                                 Education == "HNC/HND, City&Guilds level 4, NVQ/SVQ 4/5" |
                                 Education == "ONC/OND, City&Guilds level 3, NVQ/SVQ 3" |
                                 Education == "A level or equivalent" |
                                 Education == "Scottish Higher or equivalent" |
                                 Education == "GCSE A*-C, CSE grade 1, O level grade A-C" |
                                 Education == "Scottish Standard grades, Ordinary bands" |
                                 Education == "GCSE D-G, CSE grades 2-5, O level D-E" |
                                 Education == "City&Guilds level 2, NVQ/SVQ 2 and equivalent" |
                                 Education == "City&Guilds level 1, NVQ/SVQ 1 and equivalent" |
                                 Education == "Clerical and commercial qualifications" |
                                 Education == "Recognised trade apprenticeship" |
                                 Education == "Youth training certificate, skill seekers" |
                                 Education == "Other technical, professional or higher qualification" |
                                 Education == "AAT/ACA/ Accountancy qualifications"|
                                 Education == "hgv" ~ "Lower degree",
                               Education == "Not stated"|
                                 Education == "Refused" ~ "NA",
                              TRUE ~ as.character(Education))) %>%
  mutate(Education = factor(Education, levels = c("Postgraduate degree", 
                                                     "First degree",
                                                     "Lower degree",
                                                     "No qualification"), ordered = TRUE)) 

## Ethnicity

data %<>% rename(Ethnicity = y11)
data %<>% mutate(Ethnicity = case_when(Ethnicity == "English/Welsh/Scottish/Northern Irish/British" |
                                    Ethnicity == "Irish" |
                                    Ethnicity == "Gypsy or Irish Traveller" |
                                    Ethnicity == "Any other White background" |
                                    Ethnicity == "English/Welsh/Scottish/Northern Irish/British" ~ "White",
                                  Ethnicity == "Indian" |
                                    Ethnicity == "Pakistani" |
                                    Ethnicity == "Bangladeshi" |
                                    Ethnicity == "Chinese" |
                                    Ethnicity == "Any other Asian background" ~ "Asian",
                                  Ethnicity == "African" |
                                    Ethnicity == "Caribbean" |
                                    Ethnicity == "Any other Black/African/Caribbean background" ~ "African",
                                  Ethnicity == "Arab" |
                                    Ethnicity == "Any other ethnic group" |
                                    Ethnicity == "Polish" | 
                                    Ethnicity == "White and Black Caribbean" |
                                    Ethnicity == "White and Black African" |
                                    Ethnicity == "White and Asian" |
                                    Ethnicity == "Any other Mixed/Multiple ethnic background" ~ "Mixed/Other",
                                  Ethnicity == "Not stated" |
                                    Ethnicity == "Refused" ~ "NA",
                                  TRUE ~ as.character(Ethnicity))) %>%
  mutate(Ethnicity = as.factor(Ethnicity)) 
  
## Left-right self placement

data %<>% rename(Ideology = e01)
data %<>% mutate(Ideology = case_when(Ideology == "Not stated" |
                                        Ideology == "Refused" |
                                        Ideology == "Don`t know" ~ "NA",
                                      Ideology == "0 Left"  ~ "0",
                                      Ideology == "10 Right" ~ "10",
                                       TRUE ~ as.character(Ideology))) %>%
  mutate(Ideology = as.numeric(as.character(Ideology)))

## Presence of partner of respondent during interview

data %<>% rename(Partner = y48_1)
data %<>% mutate(Partner = case_when(Partner == "No" ~ "0",
                                     Partner == "Yes" ~ "1",
                                      TRUE ~ as.character(Partner))) %>%
  mutate(Partner = as.numeric(Partner))

# Save data

write.csv(data, "./bes.csv", row.names = F)
