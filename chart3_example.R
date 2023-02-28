library(ggplot2)
library(dplyr)
library(scales)
library(styler)

average_checkout <- library_df %>%
  group_by(CheckoutYear, Title) %>%
  summarize(average_checkouts = mean(Checkouts)) %>%
  group_by(CheckoutYear) %>%
  summarize(overall = mean(average_checkouts))

item_average_checkout <- library_df %>%
  filter(str_detect(Title, "(?i)The New Jim Crow")) %>%
  group_by(CheckoutYear) %>%
  summarize(item = mean(Checkouts))

Joined <- left_join(average_checkout, item_average_checkout, by = "CheckoutYear")

average_total <- gather(Joined, key = "average_type", value = "average_checkout", overall, item)

ggplot(average_total) +
  geom_line(aes(
    x = CheckoutYear,
    y = average_checkout,
    color = average_type,
  )) +
  labs(
    title = "Overall Checkout Average vs The New Jim Crow Checkout Average",
    subtitle = "In Seattle from 2017-2023",
    x = "Year",
    y = "Average of Checkouts",
    color = "Average Types"
  ) +
  scale_x_continuous(breaks = seq(2017, 2023, 1))
