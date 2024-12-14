################################################################
# build_01_starter.R
#
# author: 
# date: 
# version: class distribution
#
# notes:
# 1. if first-run: install.packages("readxl") ##done!
# 2. set working directory to local drive ##done!
# 3. data will read-in as tibbles, convert to data-frames
################################################################

# close all, clear all
rm(list=ls())
graphics.off()

# Install required packages
#install.packages("readxl") # for reading Excel files
#install.packages("dplyr")  # for data manipulation
#install.packages("ggplot2") # for plotting

# Load the necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# extract data-frames (pre-fix, pipe-install, filter-install)
prefx=read_excel("School Testing Results.xlsx",sheet=1)
pipes=read_excel("School Testing Results.xlsx",sheet=2)
filts=read_excel("School Testing Results.xlsx",sheet=3)

# convert from tibble to data-frame
prefx=as.data.frame(prefx)
pipes=as.data.frame(pipes)
filts=as.data.frame(filts)

#QUESTION N1. Flint-area schools upgraded their infrastructure in a two-step process: fixtures (pipes) and filters. Was this program successful?
## Lead results to numeric in both datasets  (also converting those values of ppb to mg/L)
prefx$`Result (mg/L)...3` = as.numeric(prefx$`Result (mg/L)...3`)
lead_levels_pre = prefx$`Result (mg/L)...3`
pipes$`Result (mg/L)...3` = as.numeric(pipes$`Result (ppb)...3`)/1000
lead_levels_pipes = pipes$`Result (mg/L)...3`
filts$`Result (mg/L)...3` = as.numeric(filts$`Result (ppb)...3`)/1000
lead_levels_filts = filts$`Result (mg/L)...3` ##LEAD LEVELS POST FILTS

## Copper results to numeric in both datasets (also converting those values of ppb to mg/L)
prefx$`Result (mg/L)...5` = as.numeric(prefx$`Result (mg/L)...5`)
copper_levels_pre = prefx$`Result (mg/L)...5`
pipes$`Result (mg/L)...7` = as.numeric(pipes$`Result (ppb)...7`)/1000 
copper_levels_pipes = pipes$`Result (mg/L)...7`
filts$`Result (mg/L)...7` = as.numeric(filts$`Result (ppb)...7`)/1000
copper_levels_filts = filts$`Result (mg/L)...7` ##COPPER LEVELS POST FILTS

#Perform t-tests to compare Pre-Fix vs. Post-Fix (Pipes Only and Pipes + Filters)
## Lead comparison (Pre-Fix vs. Pipes Only)##pese a que no son normales, se confia en que el tamano muestral sea lo suficiente confiable para que el resultado sea robusto
t_test_lead_pipes = t.test(prefx$`Result (mg/L)...3`, pipes$`Result (mg/L)...3`, 
                            alternative = "two.sided", var.equal = FALSE)


## Lead comparison (Pre-Fix vs. Pipes + Filters)
t_test_lead_filts = t.test(prefx$`Result (mg/L)...3`, filts$`Result (mg/L)...3`, 
                            alternative = "two.sided", var.equal = FALSE)

## Copper comparison (Pre-Fix vs. Pipes Only)
t_test_copper_pipes = t.test(prefx$`Result (mg/L)...5`, pipes$`Result (mg/L)...7`, 
                              alternative = "two.sided", var.equal = FALSE)

## Copper comparison (Pre-Fix vs. Pipes + Filters)
t_test_copper_filts = t.test(prefx$`Result (mg/L)...5`, filts$`Result (mg/L)...7`, 
                              alternative = "two.sided", var.equal = FALSE)

print(t_test_lead_pipes) #prefix vs only pipes (lead)
print(t_test_lead_filts) #prefix vs pipes+filter (lead)
print(t_test_copper_pipes) #prefix vs only pipes (lead)
print(t_test_copper_filts) #prefix vs only pipes (lead)

##ANSER TO QUESTION 1: IT SEEMS THAT THE PROGRAM WASN'T SUCCESSFULL OVERALL BECAUSE THE MEAN LEVELS OF LEAD AND COPPER WERE SIGNIFFICANTLY HIGHER AFTER FILTER REPLACEMENT COMPARED TO PREFIX STAGES. 
___________________________________________________________________________________________________________________

#QUESTION2A: If No, was there one stage (pipes, or filters alone) that seemed to be more effective (FOR LEAD AND COPPER LEVELS)
#JUST COMPARE ONLY THE MEAN VALUES.

_____________________________________________________________________________________________________________________________________________________________

#QUESTION3a: Riffs encouraged: Were some schools more improved than others? 

#Subseting prefx data.frame to obtean mean values of lead and copper, by school
prefx2 <- prefx %>% 
  group_by(SCH_Name) %>% 
  summarise(plomopre = mean(`Result (mg/L)...3`, na.rm = TRUE), 
            cobrepre = mean(`Result (mg/L)...5`, na.rm = TRUE))

#Subseting pipes.data frame to obtean mean values of lead and copper, by school
pipes2 <- pipes %>% 
  group_by(SCH_Name) %>% 
  summarise(plomopipes = mean(`Result (mg/L)...3`, na.rm = TRUE), 
            cobrepipes = mean(`Result (mg/L)...7`, na.rm = TRUE))

#Subseting pipes.data frame to obtean mean values of lead and copper, by school
filts2 <- filts %>% 
  group_by(SCH_Name) %>% 
  summarise(plomofilts = mean(`Result (mg/L)...3`, na.rm = TRUE), 
            cobrefilts = mean(`Result (mg/L)...7`, na.rm = TRUE))

#joining the three previous data.frames and substracting the mean measures of lead and copper, by school
schoolmeasures <- prefx2 %>% 
  full_join(pipes2, by = "SCH_Name") %>% 
  full_join(filts2, by = "SCH_Name") %>% 
  mutate(measure1lead = plomopipes - plomopre, 
         measure2lead = plomofilts - plomopre,
         measure1copper = cobrepipes - cobrepre,
         measure2copper = cobrefilts - cobrepre)

# School with the best and worst improvement according to mean differences

print(paste(
  "The school with the best results of lead mean values after post-fix is Holmes STEM Academy with a mean difference of", 
  max(schoolmeasures$measure2lead, na.rm = TRUE)
))

print(paste(
  "The school with the best results of copper mean values after post-fix is Brownell STEM Academy with a mean difference of", 
  max(schoolmeasures$measure2copper, na.rm = TRUE)
))

__________________________________________________________________________________________________________________

#QUESTION3B. some Zip Codes more impacted by pipes, versus filters?

#Subseting prefx data.frame to obtean mean values of lead and copper, by zip
prefx3 <- prefx %>% 
  group_by(SCH_Zip) %>% 
  summarise(plomoprezip = mean(`Result (mg/L)...3`, na.rm = TRUE), 
            cobreprezip = mean(`Result (mg/L)...5`, na.rm = TRUE))

#Subseting pipes.data frame to obtean mean values of lead and copper, by zip
pipes3 <- pipes %>% 
  group_by(SCH_Zip) %>% 
  summarise(plomopipeszip = mean(`Result (mg/L)...3`, na.rm = TRUE), 
            cobrepipeszip = mean(`Result (mg/L)...7`, na.rm = TRUE))

#Subseting pipes.data frame to obtean mean values of lead and copper, by zip
filts3 <- filts %>% 
  group_by(SCH_Zip) %>% 
  summarise(plomofiltszip = mean(`Result (mg/L)...3`, na.rm = TRUE), 
            cobrefiltszip = mean(`Result (mg/L)...7`, na.rm = TRUE))

#joining the three previous data.frames and substracting the mean measures of lead and copper, by school
zipmeasures <- prefx3 %>% 
  full_join(pipes3, by = "SCH_Zip") %>% 
  full_join(filts3, by = "SCH_Zip") %>% 
  mutate(measure1leadzip = plomopipeszip - plomoprezip, 
         measure2leadzip = plomofiltszip - plomoprezip,
         measure1copperzip = cobrepipeszip - cobreprezip,
         measure2copperzip = cobrefiltszip - cobreprezip)

# ZIP area with the best and worst improvement according to mean differences
print(paste(
  "The zip area with the best results of lead mean values after post-fix is Holmes STEM Academy with a mean difference of", 
  max(zipmeasures$measure2leadzip, na.rm = TRUE)
))

print(paste(
"The zip area with the best results of copper mean values after post-fix is Holmes STEM Academy with a mean difference of", 
max(zipmeasures$measure2copperzip, na.rm = TRUE)
))

