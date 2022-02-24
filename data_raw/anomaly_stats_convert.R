# Reads anomaly_stats from the xlsx, makes some checks, and saves to data folder
# as .Rdata


library(dplyr)
library(tidyr)
library(readxl)

# Make the anomaly_stat Rdata
anomaly_stats <- readxl::read_xlsx(path = "data_raw/anomaly_stats.xlsx",
                                   sheet = "anomaly_stats",
                                   na = c(NA, "NA",""),
                                   trim_ws = TRUE,
                                   col_types = c("text", rep("numeric", 15), "text", "text")) %>%
  as.data.frame()

#Check for non breaking spaces. Should all be 32 or NA
unlist(lapply(anomaly_stats[,1], function(x) utf8ToInt(gsub(pattern="[[:alnum:]/(/)/,-/./%]",x=x, replacement=""))))
unlist(lapply(anomaly_stats[,17], function(x) utf8ToInt(gsub(pattern="[[:alnum:]/(/)/,-/./%]",x=x, replacement=""))))
unlist(lapply(anomaly_stats[,18], function(x) utf8ToInt(gsub(pattern="[[:alnum:]/(/)/,-/./%]",x=x, replacement=""))))

save(anomaly_stats, file="data/anomaly_stats.RData")

