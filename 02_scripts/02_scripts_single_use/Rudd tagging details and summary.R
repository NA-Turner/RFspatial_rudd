#Rudd tag details

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
#gathering a table of Rudd tagging details for Rudd used in final analysis 
PA_rand_w_depthSAVfetch<-readRDS("01_data/02_processed_files/PA_rand_w_depthSAVfetch.rds")

#proccessed but larger telem detection dataframe 
rudd_dets<-readRDS("01_data/03_large_files_LFS/02_processed_files/Rudddets01062026.rds")
library(tidyverse)

threshold <- 5
colnames(rudd_dets)
# first and last detection per fish
fish <- rudd_dets |>
  group_by(transmitter_id) |>
  summarise(start = as.Date(min(detection_timestamp_EST)),
            end   = as.Date(max(detection_timestamp_EST))) |>
  arrange(desc(start), transmitter_id) |>
  mutate(transmitter_id = factor(transmitter_id, levels = transmitter_id))

# orange blocks: days with >= threshold fish between their first and last detection
orange <- tibble(date = seq(min(fish$start), max(fish$end), by = "day")) |>
  mutate(n_active = map_int(date, \(d) sum(fish$start <= d & fish$end >= d)),
         on  = n_active >= threshold,
         grp = consecutive_id(on)) |>
  filter(on) |>
  group_by(grp) |>
  summarise(xmin = min(date), xmax = max(date) + 1)

ggplot() +
  geom_rect(data = orange, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "#E8CFA0") +
  geom_segment(data = fish, aes(x = start, xend = end,
                                y = transmitter_id, yend = transmitter_id),
               linewidth = 0.8) +
  geom_point(data = fish, aes(start, transmitter_id), colour = "darkgreen", size = 1.8) +
  geom_point(data = fish, aes(end, transmitter_id),   colour = "darkred",   size = 1.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = paste0("Fish Detection Timelines (Orange = ", threshold, "+ Active Fish)"),
       x = "Date", y = "Transmitter ID") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
        panel.grid.minor = element_blank())


orange_periods <- orange |>
  transmute(period_start = xmin,
            period_end   = xmax - 1,
            n_days       = as.numeric(xmax - xmin))

orange_periods
#goes utnil 11-25-2025 but we cut it off beginning of sept



###rudd that are included in this analsis. tag details. 

unique(PA_rand_w_depth$transmitter_id )
#22 fish in total are included in the PA dataframe 

#details about these individuals. 
# load in the tagging workbook to get fish details 

rudd_tagworkbook<-readxl("01_data/02_processed_files/02_processed_files/Rudd_tag_workbook.xlsx")
