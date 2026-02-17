#library(eurostat)
#df <- get_eurostat("project-final/nasa_10_f_bs") # fetched on 2026-01-09
#saveRDS(df, "nasa_10_f_bs.rds")

# household portfolio data ####################################################
library(tidyverse)
df <- readRDS("project-final/nasa_10_f_bs.rds")

# F: Total financial assets/liabilities
# F2: Currency and deposits
# F5: Equity and investment fund shares
# F6: Insurance, pensions and standardised guarantees
df_filtered <- df |>
    filter(freq == "A", # annual
           unit == "MIO_EUR", # million euro
           co_nco == "CO", # consolidated
           sector == "S14", # households
           finpos == "ASS", # assets
           na_item %in% c("F", "F2", "F5", "F6"),
           geo %in% c("AT","BE","HR","CY","EE","FI","FR","DE","EL","IE",
              "IT","LV","LT","LU","MT","NL","PT","SK","SI","ES"), # euro area countries
           TIME_PERIOD == "2023-01-01" # same year as in flsdata
    ) |>
    # select relevant columns
    select(na_item, geo, values)

# create F5/F2, F6/F2, (F5+F6)/F2 ratios
df_wide <- df_filtered |>
    pivot_wider(names_from = na_item, values_from = values) |>
    mutate(F5_F2_ratio = F5 / F2) |>
    mutate(F6_F2_ratio = F6 / F2) |>
    mutate(F5F6_F2_ratio = (F5 + F6) / F2)

# financial literacy data ####################################################
flsdata <- read.csv("project-final/flsdata.csv") # fetched on 2026-01-09

library(countrycode)
flsdata_filtered <- flsdata |>
  filter(Country.or.Economy %in% c("Croatia","Cyprus","Estonia","Finland",
  "France","Germany","Greece","Ireland","Italy","Latvia","Lithuania",
  "Luxembourg","Malta","Netherlands","Portugal","Spain"))|>
  # drop columns with any NA values
  select(where(~ !any(.x == " - "))) |>
  # add country codes using countrycode package
  mutate(geo = countrycode(Country.or.Economy, origin = "country.name", destination = "eurostat")) |>
  select(-Country.or.Economy)

flsdata_filtered

# merge datasets, keeping only available countries ############################ 
final_df <- df_wide |>
    right_join(flsdata_filtered, by = "geo")

# adding household disposable income data #####################################
hdidata <- read.csv("project-final/hdidata.csv") # fetched on 2026-01-10, # estimated value for France, Germany, Greece, Netherlands, Spain

hdidata
# add eurostat country codes
hdidata <- hdidata |>
  mutate(geo = countrycode(REF_AREA, origin = "iso3c", destination = "eurostat")) |>
  select(-REF_AREA)

final_df <- final_df |>
  left_join(hdidata, by = "geo") # leaving missing values in CY, HR, MT as it is

saveRDS(final_df, "project-final/final_df.rds")

# adding gini (disposable income) data #########################################
ginidata <- read.csv("ginidata.csv") # fetched on 2026-01-13, germany data from 2022, lithuania provisional

ginidata <- ginidata |>
  mutate(geo = countrycode(Reference.area, origin = "country.name", destination = "eurostat")) |>
  select(-Reference.area)

final_df <- readRDS("final_df.rds")

final_df <- final_df |>
  left_join(ginidata, by = "geo")

saveRDS(final_df, "final_df.rds")

# visualization ##############################################################
library(tidyverse)
library(countrycode)
final_df <- readRDS("final_df.rds") |>
    # everything except geo as numeric
    mutate(across(-geo, as.numeric)) |>
    mutate(country = countrycode(geo, origin = "eurostat", destination = "country.name")) |>
    # drop CY, HR, MT rows
    filter(!country %in% c("Cyprus", "Croatia", "Malta"))
colnames(final_df)
# bubble plot with size and color

fig <-
ggplot(final_df, aes(x = Disposable.income..gross..per.capita,
       y = Subjective.financial.well.being..100.,
       size = F,
       color = ifelse(Financial.Literacy.Score..out.of.100. < 63,
          "Below OECD average",
          "Above OECD average"),
       label = paste0("[",country,"]", "\nFLS: ",
          round(Financial.Literacy.Score..out.of.100., 1),
          "\nINV: ", round(F5_F2_ratio, 2),
          "\nGini: ", round(Gini, 2)))) +
  geom_point(alpha = 0.6) +
  geom_text(nudge_y = 0, size = 1.8, fontface = "bold",
    color = "black",
    data = . %>% filter(!country %in% c("Finland", "Estonia", "Lithuania", "France"))) +
  geom_text(nudge_y = -7.5, nudge_x = -3000, size = 1.8, fontface = "bold",
    color = "black",
    data = . %>% filter(country == "Finland")) +
  geom_text(nudge_y = -7.5, nudge_x = -3000, size = 1.8, fontface = "bold",
    color = "black",
    data = . %>% filter(country == "Estonia")) +
  geom_text(nudge_y = -3, nudge_x = 0, size = 1.8, fontface = "bold",
    color = "black",
    data = . %>% filter(country == "Lithuania")) +
  geom_text(nudge_y = -3, nudge_x = 0, size = 1.8, fontface = "bold",
    color = "black",
    data = . %>% filter(country == "France")) +
  annotate("segment", x = 35500, xend = 34250-500, y = 35, yend = 30.0, color = "black", linewidth = 0.25) +
  annotate("segment", x = 24300, xend = 23050-500, y = 35, yend = 30.0, color = "black", linewidth = 0.25) +
  scale_color_manual(values = c("Below OECD average" = "salmon",
        "Above OECD average" = "skyblue")) +
  coord_cartesian(xlim = c(20000, 60000), ylim = c(20, 70)) +
  labs(
   title = "Higher Income, Higher Well-Being: Only with Financial Literacy",
   subtitle = "Household Disposable Income (Gross in US Dollars per Capita, PPP Converted) and 
Subjective Financial Well-Being Score (0-100) Across Euro-Area Countries, 2023",
   caption = "Data Source: Eurostat, OECD, OeNB\nVisualization: Takafumi Onohara",
   x = "Disposable Income",
   y = "Financial Well-Being",
   color = "Financial Literacy Score \nOut of 100",
   size = "Total Financial Assets of \nHouseholds (Million Euro)") +
  scale_size(
  #  breaks = c(118057, 507700, 3107140),
    breaks = c(100000, 500000, 3000000),
  #  labels = c("1st Qu.", "Median", "3rd Qu."),
    labels = c("   100,000", "   500,000", "3,000,000"),
    range = c(5, 25)
    ) +
  guides(color = guide_legend(order = 1, override.aes = list(size = 5)),
         size = guide_legend(order = 2)) +
  annotate("text", x = 20000, y = 62.5, label = "FLS = Financial Literacy Score (0-100) \nINV = Total Investment Ratio of Households \nGini Index in Disposable Income", 
          fontface = "italic", size = 2.5, hjust = 0) +
  geom_point(aes(x = 43922.85, y = 56.4), 
          shape = 8, size = 3, stroke = 1, color = "skyblue") +
  geom_text(aes(x = 43922.85, y = 56.4, label = "Austria"), 
          nudge_y = 2, fontface = "bold", size = 1.8, color = "black") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("bubble_plot.png", plot = fig, width = 8, height = 5, dpi = 320)

# explanation
# positive relation among countries is observed only when financial literacy is high
# the relation seems muted when financial literacy is low, or might even be negative in the lowest income range
# larger disposable income does not necessarily lead to higher subjective financial well-being (e.g., Italy, France vs. Spain, Portugal, Ireland)
# countries with large total financil assets currently with lower subjective financial well-being (e.g., Italy, France) could improve their well-being by better financial literacy, leveraging their existing financial assets given similarity in cross-country income distribution
# e.g., Italy is worse than Portugal in well-being despite having larger disposable income with the same level of income distribution, larger finnacial assets, and double in the investment ratio. Low financial literacy seems to be the main reason if pepople in Italy are relatively loose with spending or underdiversifying their risky assets
# Luxuembourg in a tricky position (outlier); explanation may lie in the cost of living in rent and essential goods that should offset the highest disposable income, Otherwise attributable to cultural aspects that make people pessimistic about their financial well-being
