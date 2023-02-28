
# Load Library 
library(ggplot2)
library(dplyr)
library(stringr)
library(styler)

# Filter The New Jim Crow to make new dataset

new_jim_crow_df <- library_df %>% 
  filter(str_detect(Title, "(?i)The New Jim Crow"))


# group by year and summarize number of checkouts per year 

book_checkout_year <- new_jim_crow_df %>% 
  group_by(CheckoutYear) %>% 
  summarize(number_of_checkouts = sum(Checkouts))

#make graph 

ggplot(book_checkout_year) +
  geom_line(aes(x = CheckoutYear, y = number_of_checkouts)) +
  labs(title = "The New Jim Crow Book Checkout Trend",
       subtitle = "In Seattle from 2017-2023",
       x = "Year", 
       y = "Total Checkouts")
