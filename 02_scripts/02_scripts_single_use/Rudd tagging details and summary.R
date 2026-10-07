#Rudd tag details

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyverse)
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

# 497  498  499  501  502  507  508  509  510  512  514  515  516 1366 1368 1370 1374 1386 1398 9149 9150 9157
#from final PA dataframe 
#22 fish in total are included in the PA dataframe 

#details about these individuals. 
# load in the tagging workbook to get fish details 

rudd_tagworkbook<-read_excel("01_data//02_processed_files/Rudd_tag_workbook.xlsx")
#filter out relevant tag ids 
tags <- c(497, 498, 499, 501, 502, 507, 508, 509, 510, 512, 514, 515, 516,
          1366, 1368, 1370, 1374, 1386, 1398, 9149, 9150, 9157)

# keep only these tags
tags_retain <- rudd_tagworkbook |> filter(TAG_ID_CODE %in% tags)

#also do that for larger telemetry dataset to get first/last detection date and number of days detected
tags_retain_rudddets <- rudd_dets |> filter(transmitter_id %in% tags)
unique(tags_retain_rudddets$transmitter_id)

tag_summary1 <- tags_retain_rudddets |>
  group_by(transmitter_id) |>
  summarise(first_detection = min(detection_timestamp_EST),
            last_detection  = max(detection_timestamp_EST),
            days_active     = as.numeric(as.Date(max(detection_timestamp_EST)) - as.Date(min(detection_timestamp_EST))) + 1,
            days_detected   = n_distinct(as.Date(detection_timestamp_EST)),
            n_detections    = n(),
            .groups = "drop") |>
  arrange(first_detection)


tag_summary1

tags_retain <- tags_retain %>% rename(transmitter_id = TAG_ID_CODE)

tag_summary_full <- tag_summary1 |>
  left_join(
    tags_retain |>
      select(transmitter_id, `Fork (mm)`, `Total (mm)`, `Mass (g)`,
             `Release Location`, `Capture Method`, Family),
    by = "transmitter_id"
  )


library(flextable)
library(officer)

ft <- tag_summary_full |>
  flextable() |>
  theme_booktabs() |>
  fontsize(size = 9, part = "all") |>
  autofit() |>
  fit_to_width(max_width = 9)   # inches; default slide is 10 in wide
save_as_docx(ft, path = "03_outputs/02_files/tag_summary.docx")
