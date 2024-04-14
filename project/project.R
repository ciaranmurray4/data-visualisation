install.packages("tidyverse")
install.packages("maps")
install.packages("plotly")
install.packages("viridis")

library(maps)
library(tidyverse)
library(plotly)
library(viridis)

metadata <- read.csv("unicef_metadata.csv")
indicator <- read.csv("unicef_indicator_2.csv")





# Visualisation 1 World Map

# Clean data for visualization one.
vis1 <- dplyr::select(indicator, country, obs_value)

# Obtain average for each country.
vis1 <- dplyr::reframe(vis1, .by = country, "Labour Force Participation Rate" = mean(.data[["obs_value"]], na.rm = TRUE))

# Get longitude and latitude for each country.
coords <- map_data("world")

# Remove unnecessary columns.
coords <- dplyr::select(coords, -order, -subregion)

# Correct mismatching country names.
coords$region[coords$region == "USA"] <- "United States"
coords$region[coords$region == "Russian Federation"] <- "Russia"
coords$region[coords$region == "Democratic Republic of the Congo"] <- "Congo, the Democratic Republic of the"
coords$region[coords$region == "Republic of Congo"] <- "Congo"
coords$region[coords$region == "Iran"] <- "Iran, Islamic Republic of"
coords$region[coords$region == "Syria"] <- "Syrian Arab Republic"
coords$region[coords$region == "Tanzania"] <- "Tanzania, United Republic of"
coords$region[coords$region == "Laos"] <- "Lao People's Democratic Republic"
coords$region[coords$region == "UK"] <- "United Kingdom"
coords$region[coords$region == "Moldova"] <- "Moldova, Republic of"
coords$region[coords$region == "North Macedonia"] <- "Macedonia, the former Yugoslav Republic of"
coords$region[coords$region == "North Korea"] <- "Korea, Democratic People's Republic of"

# Join longitude and latitude co-ordinates with data.
vis1 <- dplyr::full_join(vis1, coords, by = c("country" = "region"))

v1 <- ggplot(vis1) +
  aes(x = long, y = lat, group = group, fill = `Labour Force Participation Rate`, text = paste("Country:", country)) +
  labs(title="Labour Force Participation Rate by Country", subtitle = "2019") +
  theme_void() +
  borders() +
  scale_fill_viridis(option="viridis", direction = -1) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 1)) +
  coord_fixed(ratio = 1.205458) +
  geom_polygon()

plotly::ggplotly(v1)





# Visualisation 2 Bar Chart

# Clean unicef metadata for visualisation 2.
vis2 <- dplyr::select(metadata, country, year, `Life.expectancy.at.birth..total..years.`)

# Rename the awkward columns.
colnames(vis2) <- c("Country", "Year", "Life Expectancy")

colours <- seq(1:4)

v2 <- vis2 %>%
  # We are comparing this data to 2019 labour force participation rates
  # therefore we are only interested in 2019 data.
  filter(Year == 2019) %>%
  arrange(`Life Expectancy`) %>%
  # Filter the 35 countries with the lowest life expectancy.
  slice(1:35) %>%
  ggplot(aes(Country, `Life Expectancy`)) +
  geom_bar(stat="identity") +
  coord_flip()

plotly::ggplotly(v2)





# Visualisation 3 Scatter Plot

# Combine 2019 labour force participation rates with 2019 life expectancies.
# Get average labour force participation as we did for vis 1.
vis3 <- indicator %>%
  select(country, obs_value) %>%
  reframe(.by = country, "Labour Force Participation Rate" = mean(.data[["obs_value"]], na.rm = TRUE)) %>%
  full_join(metadata %>%
              filter(year == 2019) %>%
              select(country, `Life.expectancy.at.birth..total..years.`), by = "country") %>%
  filter(!is.na(`Life.expectancy.at.birth..total..years.`)) %>%
  filter(!is.na(`Labour Force Participation Rate`))
              

# Rename the awkward columns.
colnames(vis3) <- c("Country", "Labour Force Participation Rate", "Average Life Expectancy")

v3 <- ggplot(vis3, aes(x=`Labour Force Participation Rate`, y=`Average Life Expectancy`)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red")

plotly::ggplotly(v3)





# Visualisation 4 Time-Series Chart

# Isolate Madagascar data for the time series chart.
vis4 <- metadata %>%
  filter(country == "Madagascar") %>%
  select(country, year, `Life.expectancy.at.birth..total..years.`)

# Rename the awkward columns.
colnames(vis4) <- c("Country", "Year", "Average Life Expectancy")

v4 <- vis4 %>% 
  ggplot(aes(Year, `Average Life Expectancy`)) +
  labs(title="Average Life Expectancy Madagascar") +
  geom_line()

plotly::ggplotly(v4)