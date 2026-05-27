# Naming Conventions

This document outlines the standardized naming conventions for files, folders, and R objects in this project template.

## File and Folder Naming

### Script Files: `scriptXX-YY_descriptive_name.R`

**Format:**
- **XX** = Script class (00-99): Major workflow stage
- **YY** = Script number (01-99): Specific script within class
- All lowercase with underscores separating words

**Script Classes:**
- `script00-XX` = Setup (packages, configuration, user interface)
- `script01-XX` = Data import and formatting
- `script02-XX` = Analysis workflows
- `script03-XX` = Visualization and plotting
- `script04-XX` = Special analyses or extensions

**Examples:**
```
script00-00_user_interface.R
script00-01_load_packages.R
script01-01_import_telemetry_data.R
script01-02_import_habitat_data.R
script02-01_analysis_random_forest.R
script02-02a_analysis_repeatability.R
script02-02b_analysis_spatial_patterns.R
script03-01_plot_figures.R
```


## R Object Naming

Use consistent prefixes to indicate object type and purpose.

### Data Objects

**`data_*`** - Raw imported data, minimally processed
```r
data_hab <- read.csv("01_data/habitat_survey.csv")
data_efish_obs <- read.csv("01_data/fish_observations.csv")
data_waterlevel <- readxl::read_excel("01_data/water_data.xlsx")
```

**`df_*`** - Processed data frames (cleaned, filtered, transformed)
```r
df_hab <- data_hab %>%
  filter(!is.na(depth)) %>%
  mutate(log_depth = log(depth + 1))

df_movement <- data_det %>%
  group_by(animal_id) %>%
  summarise(distance = sum(step_length))
```

### Configuration Objects

**`param_*`** - Parameters, settings, and configuration values
```r
param_buffer_distance <- 100
param_min_detections <- 5
param_seed <- 1987
```

### Temporary Objects

**`temp_*`** - Temporary/intermediate objects (must be cleaned up)
```r
temp_data_filtered <- data %>% filter(year >= 2020)
temp_model_results <- lm(response ~ predictor, data = df)
temp_summary_stats <- df %>%
  group_by(category) %>%
  summarise(mean_value = mean(value))

# Always clean up at script end
rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n")
```

### Model Objects

Use descriptive prefixes indicating model type, followed by suffixes for content:

**Model Prefixes:**
- `RF_*` = Random Forest models
- `lm_*` = Linear models
- `glm_*` = Generalized linear models
- `glmm_*` = Generalized linear mixed models

**Common Suffixes:**
- `_model` = Fitted model object
- `_data` = Data used for modeling
- `_train` / `_test` = Training/test datasets
- `_diagnostic` = Performance metrics (often a list)
- `_CM` = Confusion matrix

**Examples:**
```r
# Linear model for depth analysis
lm_depth_model <- lm(depth ~ temperature + salinity, data = df)
lm_depth_diagnostic <- summary(lm_depth_model)
```

### Plot Objects

Store plots in lists for organization:

```r
# Create list to hold related plots
plots <- list()

# Store data and plot objects
plots$depth_plot <- ggplot(...) + ...
plots$substrate_data <- pdp::partial(...)
plots$substrate_plot <- ggplot(...) + ...
```

---

## Best Practices

### Script Organization
1. **Number by dependency**: Higher-numbered scripts depend on lower-numbered ones
2. **Descriptive names**: Include data type and analysis method in name
3. **Consistent headers**: All scripts should have standardized headers (see templates)

### Object Management
1. **Prefix consistently**: Always use appropriate prefixes for clarity
2. **Clean up temp objects**: Remove all `temp_*` objects at script end
3. **Document parameters**: Comment parameter values and their purpose

### Package Loading
1. **Core tidyverse**: Load with `library(tidyverse)` at script start
2. **Other packages**: Use explicit namespacing with `::`
   ```r
   library(tidyverse)  # Core packages

   sf::st_as_sf(data)  # Spatial operations
   corrplot::corrplot(matrix)  # Correlation plots
   ```

---

## Version History

- 2024-11-27: Initial documentation of naming conventions
