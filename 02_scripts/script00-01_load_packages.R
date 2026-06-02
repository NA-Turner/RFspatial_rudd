## --------------------------------------------------------------#
## Script name: Script0-1_load_packages
##
## Purpose of script: 
##    Load general-purpose scripts used throughout project
##    Rarely used/problematic packages may be loaded in specific scripts 
## Author:  
##
## Date Created: 
##
## --------------------------------------------------------------#  
## Modification Notes:  
##   
## --------------------------------------------------------------#

###Basic packages on CRAN
#----------------------------#
library('tidyverse') 
library('ggmap') 
library('sf') 
library('patchwork') 
library('dplyr')
library('purrr')
library('lubridate')
library('readxl')
###Source functions
#----------------------------#
source("02_scripts/01_functions/function01-01_helper_functions.R")

###Adjust Settings
#----------------------------#
theme_set(theme_classic()) #ggplot background
plots <- list() #Home for plots
