#performing network meta-analysis
library (netmeta)

# read excel
library(readxl)
Data2<-read_excel(file.choose())

# Reshaping data
library(tidyverse)
Data3<-Data2 %>%
  arrange(id, visit_date) %>%
  group_by(id)%>%
  mutate(visit=row_number())

# Reshaping data from long to wide format
library (tidyr)
# Pivoting the data
Data5 <- Data3 %>%
  spread(key = visit, value = visit_date) %>%
  # Convert presence/absence of visit_date to binary values for selected columns
  mutate_at(vars("1", "2", "3", "4"), ~ ifelse(!is.na(.), 1, 0))
Data4<-spread(Data3, key=visit, value=visit_date)

Data6 <- Data3 %>%
  arrange(id, visit_date) %>%
  group_by(id) %>%
  mutate(days_since_previous_visit = as.numeric(lead(visit_date) - visit_date,
                                                unit="days")) %>%
  ungroup()

Data6 <- Data6 %>%
  mutate(month = month(visit_date))
 

