## --------------------------------------------------------------#
## Script name: scriptXX-YY_analysis_name.R
##
## Purpose of script:
##    [Brief description of what this script does]
##
## Dependencies:
##    - script00-01_load_packages.R
##    - [List any data files or other scripts required]
##
## Author: Your Name
##
## Date Created: YYYY-MM-DD
##
## --------------------------------------------------------------#
## Modification Notes:
##   YYYY-MM-DD: [Description of changes made]
## --------------------------------------------------------------#


#####Load Data#############################################----
#-------------------------------------------------------------#

# Import raw data
# data_example <- read.csv("01 - Data/example_data.csv")
# data_spatial <- readRDS("01 - Data/spatial_data.rds")


#####Data Processing#######################################----
#-------------------------------------------------------------#

### Clean and Filter
#----------------------------#

# Process raw data into analysis-ready format
# df_example <- data_example %>%
#   filter(!is.na(value)) %>%
#   mutate(new_variable = ...)


### Summarize
#----------------------------#

# Create summary statistics
# temp_summary <- df_example %>%
#   group_by(category) %>%
#   summarise(
#     mean_value = mean(value),
#     sd_value = sd(value),
#     n = n()
#   )


#####Analysis##############################################----
#-------------------------------------------------------------#

### Model Fitting
#----------------------------#

# Set seed for reproducibility
# set.seed(param_seed)

# Fit model
# model_example <- lm(response ~ predictor1 + predictor2, data = df_example)


### Model Diagnostics
#----------------------------#

# Check model assumptions and performance
# summary(model_example)


#####Visualization#########################################----
#-------------------------------------------------------------#

### Main Plot
#----------------------------#

# Create plot
# plot_example <- ggplot(df_example, aes(x = predictor, y = response)) +
#   geom_point() +
#   theme_classic()


#####Cleanup###############################################----
#-------------------------------------------------------------#

# Remove temporary objects
rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n")


## End of script
