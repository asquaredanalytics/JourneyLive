library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

Daily_Checks <- read.csv("C:/Users/Owner/Downloads/Daily Check-ins.csv")

colnames(Daily_Checks)[colnames(Daily_Checks) == "CHECKINDATE"] <- "EVENTDATE"

Monthly_Assessments <- read.csv("C:/Users/Owner/Downloads/Monthly Assessments.csv")

colnames(Monthly_Assessments)[colnames(Monthly_Assessments) == "ASSESSMENTDATE"] <- "EVENTDATE"

Platform_Events <- read.csv("C:/Users/Owner/Downloads/Platform Events.csv")

#Comprehensive_DF <- merge(Monthly_Assessments, Platform_Events, by = c('USERID', 'EVENTDATE'), all.x = TRUE)

drop_list <- c("\\[", "\"", " ", "\\]")

pattern <- paste0(drop_list, collapse = "|")

Platform_Events$PLATFORMEVENTS <- gsub(pattern, "", Platform_Events$PLATFORMEVENTS)

Platform_Events <- Platform_Events %>%
  mutate(PLATFORMEVENTS = map_chr(str_split(PLATFORMEVENTS, ","), ~ paste(unique(str_trim(.x)), collapse = ",")))

Platform_Events <- Platform_Events %>%
  separate_longer_delim(PLATFORMEVENTS, delim = ",") %>%
  mutate(PLATFORMEVENTS = trimws(PLATFORMEVENTS), present = 1) %>%
  pivot_wider(
    names_from = PLATFORMEVENTS,
    values_from = present,
    values_fill = 0,
    values_fn = max # Ensures a 1 is kept if a word accidentally repeats in a cell
  )

Comprehensive_DF <- merge(Platform_Events, Daily_Checks, by = c('USERID', 'EVENTDATE'), all.x = TRUE)

Comprehensive_DF <- merge(Comprehensive_DF, Monthly_Assessments, by = c('USERID', 'EVENTDATE'), all.x = TRUE)

write.csv(Comprehensive_DF, "C:/Users/Owner/Downloads/Journey_Data_Merged_JUN_26.csv", row.names = FALSE)