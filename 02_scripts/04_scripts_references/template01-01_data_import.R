## --------------------------------------------------------------#
## Script name: scriptXX-YY_data_import_name.R
##
## Purpose of script:
##    Import and format raw data from [source] for downstream analysis
##
## Dependencies:
##    - script00-01_load_packages.R
##
## Author: Your Name
##
## Date Created: YYYY-MM-DD
##
## --------------------------------------------------------------#
## Modification Notes:
##   YYYY-MM-DD: [Description of changes made]
## --------------------------------------------------------------#


#####Import Raw Data#######################################----
#-------------------------------------------------------------#

# Load data files
# data_raw <- read.csv("01 - Data/raw_data.csv")
# data_metadata <- readxl::read_excel("01 - Data/metadata.xlsx")

cat("Loaded", format(nrow(data_raw), big.mark = ","), "records\n")


#####Data Cleaning#########################################----
#-------------------------------------------------------------#

### Handle Missing Values
#----------------------------#

# Check and handle NAs
# temp_na_count <- sum(is.na(data_raw))
# cat("Missing values:", temp_na_count, "\n")


### Data Type Conversions
#----------------------------#

# Convert columns to appropriate types
# temp_clean <- data_raw %>%
#   mutate(
#     date = as.Date(date, format = "%Y-%m-%d"),
#     category = as.factor(category),
#     value = as.numeric(value)
#   )


#####Data Formatting#######################################----
#-------------------------------------------------------------#

### Create Derived Variables
#----------------------------#

# Add calculated fields
# temp_formatted <- temp_clean %>%
#   mutate(
#     year = lubridate::year(date),
#     month = lubridate::month(date),
#     log_value = log(value + 1)
#   )


### Filter and Select
#----------------------------#

# Keep only relevant observations and variables
# df_final <- temp_formatted %>%
#   filter(year >= 2020) %>%
#   select(id, date, year, month, category, value, log_value)

cat("Final dataset:", format(nrow(df_final), big.mark = ","), "records\n")


#####Data Validation#######################################----
#-------------------------------------------------------------#

### Check Data Quality
#----------------------------#

# Verify data integrity
# temp_duplicates <- df_final %>%
#   group_by(id, date) %>%
#   filter(n() > 1)
#
# if(nrow(temp_duplicates) > 0) {
#   warning("Found ", nrow(temp_duplicates), " duplicate records")
# }


#####Export Processed Data (Optional)######################----
#-------------------------------------------------------------#

# Save processed data for future use (commented out - manual export only)
# saveRDS(df_final, "01 - Data/processed_data.rds")


#####Cleanup###############################################----
#-------------------------------------------------------------#

# Remove temporary objects
rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n")


## End of script
