library(tidyverse)


paths <- str_c("data/cdc_", 1:5, ".csv")

list_data <- map(paths, ~read_csv(., 
                                  col_types = cols(
                                    Year = col_double(),
                                    Quarter = col_double(),
                                    `Location Description` = col_character(),
                                    `Data Source` = col_character(),
                                    `Topic Description` = col_character(),
                                    `Measure Description` = col_character(),
                                    `Provision Group Description` = col_character(),
                                    Provision = col_character(),
                                    Value = col_character(),
                                    Citation = col_character(),
                                    `Enacted Date` = col_character(),
                                    `Effective Date` = col_character(),
                                    Comments = col_character()
                                    )
                                  ))

raw_data <- reduce(list_data, rbind)


test <- raw_data %>% 
  filter(Provision == "Bars - Statutory Preemption") %>% 
  select(-Year, -Quarter) %>% 
  distinct()













                 