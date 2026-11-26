#Rudd tag details

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyverse)
#gathering a table of Rudd tagging details for Rudd used in final analysis 
#PA_rand_w_depthSAVfetch<-readRDS("01_data/02_processed_files/PArand_complete_final.rds")

#proccessed but larger telem detection dataframe 
#larger full Rudd dataframe not used in full for the Random Forest model
# rudd_dets<-readRDS("01_data/03_large_files_LFS/02_processed_files/Rudddets01062026.rds")
#library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)


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
