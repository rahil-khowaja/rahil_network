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
 
# working on FOR Loop
cities<-c("karachi", "Lahore", "Islamabad", "Sanghar", "badin")

for (city in cities)(
  print(city)
)

for (i in 1:length(cities)){
  print(paste(cities[i], "is present at the number", i))
}

# Write your own function
sum_div<-function(x, y=1) {
  z<-(x*y+x/y)
  return(z)
}
sum_div(5, 0)
sum_div(0, 5)

sum_div<-function(x, y){
  if (y==0 | x==0) {
    return(0)
  }
  x*y+x/y
}

# function for creating character to factor variable
charac_factor<-function(Data){
  num_columns<-sapply(Data, is.character)
  data[num_columns]<-lapply(Data[num_columns], as.factor)
  return(Data)
}

# Change numeric values into factors
num_factors<-function(Data){
  num_var<-sapply(Data, is.numeric)
  for (col in names(Data[num_var])){
    if(length(unique(Data[[col]]))<=6){
      Data[[col]]<-as.factor(Data[[col]])
    }
    }
  return(Data)
}

# create a function to write mean, SD, median, minimum and maximum
summarization<-function(Data, x){
  mean_value<-mean(Data[[x]])
  sd_value<-sd(Data[[x]])
  min_value<-min(Data[[x]])
  result<-data.frame(mean=mean_value, SD=sd_value, Minimun=min_value)
  return(result)
}
summarization(Data=Data6, x="Age")

# create a function to write mean, SD
summarization<-function(Data, x){
  table<-Data %>%
    summarize(Mean=mean(!!sym(x)),
              SD=sd(!!sym(x)))
  return(table)
}
summarization(Data=Data6, x="Age")

# Create a function to write mean, SD on the basis of group variable
library(dplyr)

summarize_group <- function(Data, x, y) {
  table <- Data %>%
    group_by(!!sym(y)) %>%
    summarize(Mean = mean(!!sym(x), na.rm = TRUE),
              SD = sd(!!sym(x), na.rm = TRUE))
  return(table)
}
summarize_group(Data6, x="Age", y="Group")

# Create a function to write mean, SD for all continous variables in a dataset
summarization <- function(Data) {
  cont_columns <- sapply(Data, is.numeric)
  variable <- names(Data)[cont_columns]
  Mean <- sapply(Data[cont_columns], mean)
  SD <- sapply(Data[cont_columns], sd)
  table <- data.frame(Names = variable, Mean = Mean, SD = SD)
  return(table)
}
summarization(Data6)

# 
install.packages("DataExplorer")
library(DataExplorer)
library(tidyverse)
Data6 %>%
  select(Age, Group)%>%
  create_report(output_file = "final.report",
                output_dir   = "C:/Users/khowa/Desktop/Studies/rahil_network1",
                y="Age",
                report_title="Report")

final_data<-read.csv("C:/Users/khowa/Desktop/Studies/rahil_network1/Book1.csv")

install.packages("GGally")
library(GGally)
ggpairs(final_data)
ggcorr(final_data)
ggally_compare(final_data)
library(ggplot2)

ggplot(final_data, aes(x = HR_OS, y = HR_PFS)) +
  geom_point(color = 'blue', size = 5, shape=3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add linear regression line
  facet_wrap(~ Group1) +  
  labs(title = "PFS and OS",
       x = "PFS",
       y = "OS")

# Assuming final_data is your dataset
cor_matrix <- cor(final_data[c("HR_OS", "SE_OS", "HR_PFS", "SE_PFS")])
ggcorr(final_data,
       method = c("pairwise", "pearson"),
       cor_matrix = NULL,
       nbreaks = NULL,
       digits = 2,
       name = "Rahil",
       geom="text",
       low = "green",
       mid = "orange",
       high = "red",
       label_round = TRUE
)

ggally_density(final_data, aes(x=HR_PFS, y=SE_PFS))
