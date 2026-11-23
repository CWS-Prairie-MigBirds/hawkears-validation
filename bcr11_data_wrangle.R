## Download all Grassland and Parkland projects for HawkEars analysis
# Janine McManus

#1. Load libraries----

library(tidyverse)
library(wildrtrax)
library(data.table)

#2. Login to WildTrax----

config <- "wt_login.R"
source(config)
wt_auth()

#3. Download projects table and filter to the ones needed

# right now I have done just morning recordings, and I've included paired recordings
# for birds canada and CWS. Could add Sam's AB stuff and grasslands without too much pain

projects <- wt_get_projects(sensor = "ARU")

projs_HE <- projects %>% filter(project_id %in% c(3749, 3785, 3984, 3986, 
                                                  2326, 1287, 2302, 829,
                                                  1094, 1093, 638, 1149, 1137,
                                                  1966, 639, 3532))
#4. Download the data

#This line will take a minute to run while it downloads the data

wt_main = purrr::map(.x = unique(projs_HE$project_id),
                     .f = ~wt_download_report(project_id = .x, sensor_id = "ARU",
                                              reports = c("main")))

wt_ai = purrr::map(.x = unique(projs_HE$project_id),
                     .f = ~wt_download_report(project_id = .x, sensor_id = "ARU",
                                              reports = c("ai")))

df_main <- rbindlist(wt_main, fill = TRUE)

check <- df_main %>% filter(is.na(task_method))


df_ai <- rbindlist(wt_ai, fill = TRUE)

list <- list(df_ai, df_main)

saveRDS(list, "BCR11_data_HE.rds")

