library(tidyverse)
library(plotly)
library(ggstatsplot)

df <- read_csv('forDash.csv')

# boxplot(df$price)$out

# ggbetweenstats(df, listingType, price, outlier.tagging = TRUE)

# ggstatsplot::ggbetweenstats(
#   data = df,
#   x = listingType,
#   y = price,
#   type = "nonparametric",
#   plot.type = "box",
#   xlab = "Listing Type",
#   ylab = "Price",
#   pairwise.comparisons = TRUE,
#   p.adjust.method = "fdr",
#   outlier.tagging = TRUE,
#   # outlier.label = centrum,
#   ggtheme = ggplot2::theme_grey(),
#   ggstatsplot.layer = FALSE
# )


df %>%
  ggplot(aes(totalArea,pricePMsq)) +
  geom_point()+
  geom_smooth(orientation = 'y')+
  ylim(0,10000)
