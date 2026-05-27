## --------------------------------------------------------------#
## Script name: Script0-1_load_packages
##
## Purpose of script: 
##    Load general-purpose scripts used throughout project
##    Rarely used/problematic packages may be loaded in specific scripts 
##    custom telemetrytoolsFESL needs to be installed with devtools
##
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
library('tidyverse') #Basic organizing and plotting
library('ggmap') #google basemaps
library('sf') #handle shapefiles, projections, and distance calculations
library('patchwork') #Add plots together

###Packages off CRAN
#----------------------------#
###Lab package
#Only need to install during updates
devtools::install_github("FishEcologyScience/FESLtelemetry")
library('FESLtelemetry')
#library('telemetrytoolsFESL')


###GLATOS package
#remotes::install_github("jsta/glatos")
#library('glatos')

###Source functions
#----------------------------#
source("02_scripts/01_functions/function01-01_helper_functions.R")

###Adjust Settings
#----------------------------#
theme_set(theme_classic()) #ggplot background
options(scipen=999) #Drop scientific notation
param_seed <- 1987 #Set seed for randomized analysis

plots <- list() #Home for plots
