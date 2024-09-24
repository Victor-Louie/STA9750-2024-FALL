if(!require("tidyverse")) install.packages("tidyverse")

# Let's start with Fare Revenue
library(tidyverse)
if(!file.exists("2022_fare_revenue.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_fare_revenue.xlsx" in your project
  # directory.
  download.file("http://www.transit.dot.gov/sites/fta.dot.gov/files/2024-04/2022%20Fare%20Revenue.xlsx", 
                destfile="2022_fare_revenue.xlsx", 
                quiet=FALSE, 
                method="wget")
}
FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`, 
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`)

# Next, expenses
if(!file.exists("2022_expenses.csv")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_expenses.csv" in your project
  # directory.
  download.file("https://data.transportation.gov/api/views/dkxx-zjd6/rows.csv?date=20231102&accessType=DOWNLOAD&bom=true&format=true", 
                destfile="2022_expenses.csv", 
                quiet=FALSE, 
                method="wget")
}
EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, 
         `Agency`,
         `Total`, 
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()
# Monthly Transit Numbers
library(tidyverse)
if(!file.exists("ridership.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "ridership.xlsx" in your project
  # directory.
  download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-09/July%202024%20Complete%20Monthly%20Ridership%20%28with%20adjustments%20and%20estimates%29_240903.xlsx", 
                destfile="ridership.xlsx", 
                quiet=FALSE, 
                method="wget")
}
TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs
MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="VRM") |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`, 
           `Mode`, `3 Mode`, month) |>
  rename(`metro_area` = `UZA Name`)|>
  summarize(VRM = sum(VRM)) |>
  filter(`VRM`== max(`VRM`))|>
  ungroup() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))
FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))
## This code needs to be modified
USAGE <- USAGE |>
  mutate(Mode=case_when(
    Mode == "HR" ~ "Heavy Rail", 
    Mode == "LR" ~ "Light Rail",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "CC" ~ "Cable Car",
    Mode == "MG" ~ "Monorail/Automated Guideway",
    Mode == "FB" ~ "Ferryboats",
    Mode == "TR" ~ "Aerial Tramways",
    Mode == "MB" ~ "Bus",
    Mode == "TB" ~ "Trolleybus",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "PB" ~ "Publico",
    Mode == "DR" ~ "Demand Reponse",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "AR" ~ "Alaska Railroad",
    TRUE ~ "Unknown"))
if(!require("DT")) install.packages("DT")
library(DT)
sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()

### What transit agency had the most total VRM in our data set?
# Agency_VRM <- USAGE |>
#   group_by(`Agency`)|>
#   summarize(total_VRM = sum(VRM, na.rm = TRUE))
# 
# max_VRM <- max(Agency_VRM$total_VRM, na.rm = TRUE)
# 
# top_Agency <- Agency_VRM |>
#   filter(total_VRM == max_VRM)
# 
# print(top_Agency)
### MTA New York City Transit had the most total VRM in this sample

### What transit mode had the most total VRM in our data set?
# Mode_VRM <- USAGE |>
#   group_by(`Mode`)|>
#   summarize(total_VRM = sum(VRM, na.rm = TRUE))
# 
# max_VRM <- max(Mode_VRM$total_VRM, na.rm = TRUE)
# 
# top_mode <- Mode_VRM |>
#   filter(total_VRM == max_VRM)
# 
# print(top_mode)
### BUS had the most total VRM in this sample

### How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
# nyc_subway_may_trips <- TRIPS |>
#   filter(Mode == "HR")|>
#   filter(Agency == 'MTA New York City Transit')|>
#   filter(month == "2024-05-01")|>
#   select(UPT)
# print(nyc_subway_may_trips)
### 180,458,819 trips were taken on the NYC Subway (Heavy Rail) in May 2024

### How much did NYC subway ridership fall between April 2019 and April 2020?
# nyc_subway_april_trips <- TRIPS |>
#   filter(Mode == 'HR')|>
#   filter(Agency == 'MTA New York City Transit')|>
#   filter(between(month, as.Date("2019-04-01"), as.Date("2020-04-01")))|>
#   summarize(UPT = sum(UPT, na.rm = TRUE))
# print(nyc_subway_april_trips)
### 2,706,415,856 subway ridership fall between April 2019 and April 2020

### What metro area had the most total VRM in our data set?
# metro_area_VRM <- USAGE |>
#   group_by(`metro_area`)|>
#   summarize(total_VRM = sum(VRM, na.rm = TRUE))
# 
# max_VRM <- max(metro_area_VRM$total_VRM, na.rm = TRUE)
# 
# top_metro_area <- metro_area_VRM |>
#   filter(total_VRM == max_VRM)
# 
# print(top_metro_area)
### New York--Jersey City--Newark, NY--NJ had the most total VRM in this sample

### How much UPT were taken on the Light Rail in November 2023?
# nyc_subway_may_trips <- TRIPS |>
#   filter(Mode == "LR")|>
#   filter(month == "2023-11-01")|>
#   summarize(UPT = sum(UPT, na.rm = TRUE))
# print(nyc_subway_may_trips)
### 27,166,436 UPT were taken on the Light Rail in November 2023

### How much did NYC subway ridership fall between March 2020 and March 2022?
# nyc_subway_march_trips <- TRIPS |>
#   filter(Mode == 'HR')|>
#   filter(Agency == 'MTA New York City Transit')|>
#   filter(between(month, as.Date("2020-03-01"), as.Date("2022-03-01")))|>
#   summarize(UPT = sum(UPT, na.rm = TRUE))
# print(nyc_subway_march_trips)
### 2,341,451,028 subway ridership fall between March 2020 and March 2022

library(lubridate)

USAGE_2022_ANNUAL <- USAGE |>
  filter(year(month) == 2022) |>
  group_by(`NTD ID`, `Agency`, `metro_area`, `Mode`) |>
  summarize(
    UPT = sum(UPT, na.rm = TRUE),
    VRM = sum(VRM, na.rm = TRUE)
  ) |>
  ungroup()
print(USAGE_2022_ANNUAL)

USAGE_AND_FINANCIALS <- left_join(USAGE_2022_ANNUAL,
                                  FINANCIALS,
                                  join_by(`NTD ID`, Mode)) |>
  drop_na()

financials_summary <- USAGE_AND_FINANCIALS |>
  group_by(`Agency`, `Mode`) |>
  summarize(total_UPT = sum(UPT, na.rm = TRUE)) |>
  ungroup()

# Find the maximum UPT
max_UPT <- max(financials_summary$total_UPT, na.rm = TRUE)

# Identify the transit system (agency and mode) with the maximum UPT
top_transit_system <- financials_summary |>
  filter(total_UPT == max_UPT)

print(top_transit_system)