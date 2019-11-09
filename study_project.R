#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("sqldf")
library(tidyverse)
library(dplyr)
library(sqldf)

setwd('D:/R projects/Study project')
options(scipen = 999, digits=4)

clean_loan_stats_file <- function(data)
{
  data_colnames <- get_column_names(data)
  print(paste(format(get_date_time_stamp()),'-> Column names were read.'))
  
  data_types <- get_column_types(data)
  print(paste(format(get_date_time_stamp()),'-> Column types were read'))
  
  print(count_column_by_types(data_types))
  
  numeric_data <- get_numeric_integer_columns(data)
  print(paste(format(get_date_time_stamp()),'-> Numeric / integer data were obtained.'))
  
  factor_data <- select(data, which(sapply(data,is.factor)))
  print(paste(format(get_date_time_stamp()),'-> Factor data were obtained.'))
  
  numeric_data_summary <- summarise_numeric_data(numeric_data)
  print(paste(format(get_date_time_stamp()),'-> Numeric / integer data summary were obtained.'))
  
  na_stats <- get_na_statistics(data, data_colnames, data_types)
  print(paste(format(get_date_time_stamp()),'-> NA statistics were obtained.'))
  print(na_stats)
  
  print(paste(format(get_date_time_stamp()),'-> Columns with NA ratio greater than 0.75.'))
  print(na_stats %>% filter(na_stats$na_ratio > 0.75))
  
  print(paste(format(get_date_time_stamp()),'-> Columns with NA ratio greater than 0 and less than /equal to 0.75.'))
  print(na_stats %>% filter(na_stats$na_ratio !=0 & na_stats$na_ratio <= 0.75))
  
  print(paste(format(get_date_time_stamp()),'-> NA ratio for columns of \'factor\' type'))
  print(na_stats %>% filter(na_stats$data_type == "factor"))
  
  print(paste(format(get_date_time_stamp()),'-> Merging NA statistics with numeric / integer data summary...'))
  merged_statistics <- merge(na_stats, 
                   numeric_data_summary, 
                   by.x="column_name", 
                   by.y="Col", 
                   all.x=TRUE) 
  print(merged_statistics)
  
  numeric_data_meadina_mean <- get_meadian_and_mean_numeric_data(numeric_data)
  print(paste(format(get_date_time_stamp()),'-> Got mean and median for numeric / integer data summary.'))
  
  #select median and mean for numeric data without columns names - so we will be able identify doubled data
  numeric_data_meadina_mean_no_colnames <- numeric_data_meadina_mean[,2:3]
  
  doubled_columns_name_to_remove <- define_columns_with_similar_data(numeric_data_meadina_mean, numeric_data_meadina_mean_no_colnames)
  print(paste(format(get_date_time_stamp()),'-> Columns with doubled data defined:'))
  print(doubled_columns_name_to_remove)
  
  columns_name_to_remove <- doubled_columns_name_to_remove
  
  #'numeric', 'integer', 'logical' columns where NA is more than 95% - to be removed from data set
  merged_statistics_failed_numbers_remove <- filter(merged_statistics, merged_statistics$na_ratio > 0.95 
                                                    & merged_statistics$data_type %in% c('numeric', 'integer', 'logical'))
  print(paste(format(get_date_time_stamp()),'-> Columns with NA ratio 95% defined:'))
  print(merged_statistics_failed_numbers_remove)
  
  print(paste(format(get_date_time_stamp()),'-> Get factor column names to remove...'))
  factor_columns_name_to_remove <- get_factor_columns_to_remove(factor_data)
  print(paste(format(get_date_time_stamp()),'-> Got factor column names to remove.'))
  
  #combine 3 vectors with column names to remove
  columns_name_to_remove <- c(sapply(merged_statistics_failed_numbers_remove$column_name, as.character), 
                              columns_name_to_remove,
                              factor_columns_name_to_remove)
  
  print(paste(format(get_date_time_stamp()),'-> Removing columns and replacing data...'))
  data <- remove_columns(data, columns_name_to_remove) %>% 
    substitute_na_by_max_value(merged_statistics, 0.10, 0.95) %>% 
    substitute_na_by_max_value(merged_statistics, 0.10, 0.95) %>% 
    substitute_na_by_min_value(merged_statistics, 0.00, 0.10) %>% 
    substitute_empty_values()
  print(paste(format(get_date_time_stamp()),'-> Removing columns and replacing data were finished.'))
 
  #print(paste(format(get_date_time_stamp()),'-> Printing factor data summary...'))
  #print_factor_data_summary(factor_data)
  #print(paste(format(get_date_time_stamp()),'-> Finished printing factor data summary.'))
  
  print(paste(format(get_date_time_stamp()),'-> Finished cleaning. Exiting with code 0.'))
  return(data)
}

get_date_time_stamp <- function()
{
  return(format(Sys.time(), "%d %b %Y %X"))
}

get_column_names <- function(data)
{
  return(colnames(data, do.NULL = TRUE, prefix = "col"))
}

get_column_types <- function(data)
{
  return(sapply(data, class))
}

count_column_by_types <- function(data_types)
{
  return (table(data_types))
}

get_numeric_integer_columns <- function(data)
{
  return (select(data, which(sapply(data,is.numeric))))
}

summarise_numeric_data <- function(numeric_data)
{
  numeric_data_summary <- as_tibble(numeric_data) %>%
    gather(Col, Value) %>% 
    group_by(Col) %>% 
    summarise(Minimum = min(Value, na.rm = TRUE),
              FirstQu = quantile(Value, probs = 0.25, na.rm = TRUE),
              Median = median(Value, na.rm = TRUE),
              Mean = mean(Value, na.rm = TRUE),
              ThirdQu = quantile(Value, probs = 0.75, na.rm = TRUE),
              Maximum = max(Value, na.rm = TRUE),
              Unique = length(unique(Value, na.rm = TRUE)))
  return (numeric_data_summary)
}

get_na_statistics <- function(data, data_colnames, data_types)
{
  #the number of na data in a column
  data_na_count <- data %>% sapply(function(name) sum(is.na(name))) 
  
  #the number of non na data in a column
  data_non_na_count <- data %>% sapply(function(name) sum(!is.na(name)))
  
  #column name, type, na number, non na number
  columns_data <- do.call(rbind, Map(data.frame,
                                     column_name = data_colnames,
                                     data_type=data_types, 
                                     non_na_count= data_non_na_count, 
                                     na_count=data_na_count)
                          )
  
  c1 <- columns_data$na_count
  number_of_rows <- nrow(data)
  columns_data$na_ratio <- c1 / number_of_rows
  
  #remove row names in a data frame
  rownames(columns_data) <- c()
  
  return(columns_data)
}

get_meadian_and_mean_numeric_data <- function(numeric_data)
{
  result <- as_tibble(numeric_data) %>%
  gather(Col, Value) %>% 
  group_by(Col) %>% 
  summarise(Median = median(Value, na.rm = TRUE),
            Mean = mean(Value, na.rm = TRUE))
  return(result)
}

define_columns_with_similar_data <- function(numeric_comparison_data, numeric_comparison_data_no_colnames)
{
  #get doubled data using SQL syntax on R dataframes
  doubledValue <- sqldf('SELECT COUNT(*), Median, Mean FROM numeric_comparison_data_no_colnames
                      GROUP BY Median, Mean HAVING COUNT(*)>1') %>% .[,2:3]
  
  #filter data by doubled median and mean, group by one of the parameters (Median) and take columns to save in dataframe
  doubled_columns_to_save <- numeric_comparison_data %>% 
    filter(Median %in% doubledValue$Median & Mean %in% doubledValue$Mean) %>% 
    group_by(Median) %>% 
    slice(head(row_number(), 1)) %>% 
    .[,1] %>% 
    pull(Col)
  
  #define all doubled rows
  doubled_rows_all <- numeric_comparison_data %>% 
    filter(Median %in% doubledValue$Median & Mean %in% doubledValue$Mean) 
  
  doubled_columns_name_to_remove <- filter(doubled_rows_all, !(Col %in% doubled_columns_to_save) ) %>%  pull(Col)
  return(doubled_columns_name_to_remove)
}

remove_columns <- function(data, column_names)
{
  return(data[ , !(names(data) %in% column_names)])
}

substitute_na_by_max_value <- function(data, na_stats_data, min_ratio, max_ratio)
{
  #numeric columns to be replaced by maximum value
  mergeDF_failed_numbers_replace <- filter(na_stats_data, na_stats_data$na_ratio > min_ratio
                                           & na_stats_data$na_ratio <= max_ratio 
                                           & na_stats_data$data_type %in% c('numeric', 'integer'))

  data <- data %>%
    mutate_at(as.vector(mergeDF_failed_numbers_replace$column_name), ~replace(., is.na(.), max(., na.rm=TRUE)))

  return(data)
}

substitute_na_by_min_value <- function(data, na_stats_data, min_ratio, max_ratio)
{
  #numeric columns to be replaced by minimum value
  mergeDF_failed_numbers_replace <- filter(na_stats_data, na_stats_data$na_ratio > min_ratio 
                                           & na_stats_data$na_ratio <= max_ratio 
                                           & na_stats_data$data_type %in% c('numeric', 'integer'))
  data <- data %>%
    mutate_at(as.vector(mergeDF_failed_numbers_replace$column_name), ~replace(., is.na(.), min(., na.rm=TRUE)))
}

print_factor_data_summary <- function(factor_data)
{
  factor_data_summary <- as_tibble(factor_data) %>%
    gather(Col, Value) %>% 
    group_by(Col) %>% 
    summarise(Unique = length(unique(Value, na.rm = TRUE)))
  
  summary_row_count <- nrow(factor_data_summary)
  
  print(factor_data_summary, n=summary_row_count)
}

get_factor_columns_to_remove <- function(factor_data)
{
  date_data <- factor_data[ , grepl( "_d" , names( factor_data ) ) ]
  colnames_to_remove <- c(colnames(date_data), 'earliest_cr_line', 'emp_title')
  return(colnames_to_remove)
}

substitute_empty_values <- function(factor_data)
{
  
  factor_data$revol_util[factor_data$revol_util == ""] <- "0%"
  table(as.vector(factor_data$revol_util))
  
  factor_data$verification_status_joint[factor_data$verification_status_joint == ""] <- "Not Verified"
  table(as.vector(factor_data$verification_status_joint))
  
  return(factor_data)
}


file_name <- "LoanStats_2017Q1.csv"
print(paste(format(get_date_time_stamp()),'-> Try to read', file_name, '...'))
data <- read.csv(file_name)
print(paste(format(get_date_time_stamp()),'-> File', file_name, 'was read.'))

data1 <- clean_loan_stats_file(data)

#1. term - OK
#2. int_rate - OK
#3. grade - OK
#4. sub_grade - OK
#5. emp_title - diverse, to remove +
#6. emp_length - NA 6697
#7. home_ownership - OK
#8. verification_status - OK
#9. issue_d - to remove (DATES) +
#10. loan_status - OK
#11. pymnt_plan - OK
#12. purpose - OK
#13. title - OK
#14. zip_code - OK
#15. addr_state - OK
#16. earliest_cr_line - to remove (DATES) +
#17. revol_util - change to 0% if whitespace or empty +
#18. initial_list_status - OK
#19. last_pymnt_d - to remove (DATES) +
#20. next_pymnt_d - to remove (DATES) +
#21. last_credit_pull_d - to remove(DATES) +
#22. application_type - OK
#23. verification_status_joint - change to 'Not verified' if whitespace or empty +
#24. sec_app_earliest_cr_line - to remove(DATES)
#25. hardship_flag - OK - to remove?
#26. hardship_type - OK - to remove?
#27. hardship_reason - OK
#28. hardship_status - OK
#29. hardship_start_date - to remove(DATES) 
#30. hardship_end_date - to remove(DATES) 
#31. payment_plan_start_date - to remove(DATES) 
#32. hardship_loan_status - OK
#33. disbursement_method - OK
#34. debt_settlement_flag - OK
#35. debt_settlement_flag_date - to remove(DATES) 
#36. settlement_status - OK - to remove?
#37. settlement_date - to remove(DATES) 