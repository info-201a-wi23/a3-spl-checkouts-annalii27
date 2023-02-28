library(scales)
library(plotly)
library(tidyr)
library(styler)

# Usage Type over years (only comparing book and e-book)

type_checkout_df <- library_df %>%
  select(UsageClass, CheckoutYear, Checkouts) %>%
  group_by(UsageClass, CheckoutYear) %>%
  summarize(CheckoutTotal = sum(Checkouts))

checkout_plot <- ggplot(type_checkout_df) +
  geom_col(mapping = aes(
    x = CheckoutYear,
    y = CheckoutTotal,
    fill = UsageClass,
    text = paste("Total Checkouts:", CheckoutTotal)
  )) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Digital vs Physical Checkouts",
    subtitle = "In Seattle from 2017-2023",
    x = "Year",
    y = "Number of Checkouts (millions)",
    fill = "Type of Checkout"
  ) +
  scale_y_continuous(labels = label_number_si())

ggplotly(checkout_plot, tooltip = "text")
