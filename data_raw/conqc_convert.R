# Reads conqc from the xlsx, makes some checks, and saves to data folder
# as .Rdata

library(dplyr)
library(tidyr)
library(readxl)

# Make the conqc Rdata
conqc <- readxl::read_xlsx(path = "data_raw/conqc.xlsx",
                           sheet = "conqc",
                           na = c(NA, "NA",""),
                           trim_ws=TRUE,
                           col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "text", "text")) %>%
  as.data.frame()

#Check for non breaking spaces. Should all be 32 or NA
unlist(lapply(conqc[,1], function(x) utf8ToInt(gsub(pattern="[[:alnum:]/(/)/,-/./%]",x=x, replacement=""))))
unlist(lapply(conqc[,2], function(x) utf8ToInt(gsub(pattern="[[:alnum:]/(/)/,-/./%]",x=x, replacement=""))))
unlist(lapply(conqc[,7], function(x) utf8ToInt(gsub(pattern="[[:alnum:]/(/)/,-/./%]",x=x, replacement=""))))
unlist(lapply(conqc[,8], function(x) utf8ToInt(gsub(pattern="[[:alnum:]/(/)/,-/./%]",x=x, replacement=""))))

save(conqc, file="data/conqc.RData")


