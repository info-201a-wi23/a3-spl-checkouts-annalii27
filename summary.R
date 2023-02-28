
# Which month in 2020 had the highest number of checkouts for The New Jim Crow (BLM)?
# How did the number of The New Jim Crow checkouts change over time (year)? 
# What is the year with the most checkouts for digital (COVID)?
# Which year had the most checkouts?
# What is the average number of checkouts for each item? (compare to NJC)

library(dplyr)
library(tidyr)

# base dateframes
new_jim_crow_df <- library_df %>% 
  filter(str_detect(Title, "(?i)The New Jim Crow"))
book_checkout_year <- new_jim_crow_df %>% 
  group_by(CheckoutYear) %>% 
  summarize(number_of_checkouts = sum(Checkouts))

# highest monthly checkout in 2020
item_month_highest_checkout <- new_jim_crow_df %>% 
  filter(CheckoutYear == "2020") %>% 
  group_by(CheckoutMonth) %>% 
  summarise(monthly_checkout = sum(Checkouts)) %>% 
  filter(monthly_checkout == max(monthly_checkout)) %>% 
  pull(CheckoutMonth) 

number_item_month_highest_checkout <- new_jim_crow_df %>% 
  filter(CheckoutYear == "2020") %>% 
  group_by(CheckoutMonth) %>% 
  summarise(monthly_checkout = sum(Checkouts)) %>% 
  filter(monthly_checkout == max(monthly_checkout)) %>% 
  pull(monthly_checkout)


# highest and lowest yearly checkout
highest_item_checkout_year <- book_checkout_year %>%
  filter(number_of_checkouts == max(number_of_checkouts)) %>% 
  pull(number_of_checkouts)

lowest_item_checkout_year <- book_checkout_year %>%
  filter(number_of_checkouts == min(number_of_checkouts)) %>% 
  pull(number_of_checkouts) 

highest_item_year <-  book_checkout_year %>%
  filter(number_of_checkouts == max(number_of_checkouts)) %>% 
  pull(CheckoutYear)

lowest_item_year <- book_checkout_year %>%
  filter(number_of_checkouts == min(number_of_checkouts)) %>% 
  pull(CheckoutYear)

# highest checkout number throughout years

highest_checkout_year <- library_df %>% 
  group_by(CheckoutYear) %>% 
  summarize(checkout_per_year = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(checkout_per_year == max(checkout_per_year)) %>% 
  pull(CheckoutYear)

# highest digital checkout throughout years
highest_digital_checkout_year <- library_df %>% 
  filter(UsageClass == "Digital") %>% 
  group_by(CheckoutYear) %>% 
  summarize(checkout_per_year = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(checkout_per_year == max(checkout_per_year)) %>% 
  pull(CheckoutYear)

# average library checkout for items
average_checkout <- library_df %>% 
  group_by(Title) %>% 
  summarize(average_checkouts = mean(Checkouts)) %>% 
  summarize(overall_average = mean(average_checkouts)) %>% 
  pull(overall_average)
  
# average library checkout for The New Jim Crow
item_average_checkout <- library_df %>% 
  filter(str_detect(Title, "(?i)The New Jim Crow")) %>% 
  summarize(average_checkouts = mean(Checkouts)) %>% 
  summarize(overall_average = mean(average_checkouts)) %>% 
  pull(overall_average)

  
  
  
  
  
  
  
  
  


