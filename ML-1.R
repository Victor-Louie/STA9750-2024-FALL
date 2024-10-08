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
### 2: Recoding the Mode column
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

## 3A.What transit agency had the most total VRM in our data set?
Agency_VRM <- USAGE |>
  group_by(`Agency`)|>
  summarize(total_VRM = sum(VRM, na.rm = TRUE))

max_VRM <- max(Agency_VRM$total_VRM, na.rm = TRUE)

top_Agency <- Agency_VRM |>
  filter(total_VRM == max_VRM)

print(top_Agency)
## MTA New York City Transit had the most total VRM in this sample

## 3B.What transit mode had the most total VRM in our data set?
Mode_VRM <- USAGE |>
  group_by(`Mode`)|>
  summarize(total_VRM = sum(VRM, na.rm = TRUE))

max_VRM <- max(Mode_VRM$total_VRM, na.rm = TRUE)

top_mode <- Mode_VRM |>
  filter(total_VRM == max_VRM)

print(top_mode)
## BUS had the most total VRM in this sample

## 3C.How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
nyc_subway_may_trips <- TRIPS |>
  filter(Mode == "HR")|>
  filter(Agency == 'MTA New York City Transit')|>
  filter(month == "2024-05-01")|>
  select(UPT)
print(nyc_subway_may_trips)
## 180,458,819 trips were taken on the NYC Subway (Heavy Rail) in May 2024

## 3E.How much did NYC subway ridership fall between April 2019 and April 2020?
nyc_subway_april_trips <- TRIPS |>
  filter(Mode == 'HR')|>
  filter(Agency == 'MTA New York City Transit')|>
  filter(between(month, as.Date("2019-04-01"), as.Date("2020-04-01")))|>
  summarize(UPT = sum(UPT, na.rm = TRUE))
print(nyc_subway_april_trips)
## 2,706,415,856 subway ridership fall between April 2019 and April 2020

## 4A.What metro area had the most total VRM in our data set?
metro_area_VRM <- USAGE |>
  group_by(`metro_area`)|>
  summarize(total_VRM = sum(VRM, na.rm = TRUE))

max_VRM <- max(metro_area_VRM$total_VRM, na.rm = TRUE)

top_metro_area <- metro_area_VRM |>
  filter(total_VRM == max_VRM)

print(top_metro_area)
## New York--Jersey City--Newark, NY--NJ had the most total VRM in this sample

## 4B.How much UPT were taken on the Light Rail in November 2023?
nyc_subway_may_trips <- TRIPS |>
  filter(Mode == "LR")|>
  filter(month == "2023-11-01")|>
  summarize(UPT = sum(UPT, na.rm = TRUE))
print(nyc_subway_may_trips)
## 27,166,436 UPT were taken on the Light Rail in November 2023

## 4C.How much did NYC subway ridership fall between March 2020 and March 2022?
nyc_subway_march_trips <- TRIPS |>
  filter(Mode == 'HR')|>
  filter(Agency == 'MTA New York City Transit')|>
  filter(between(month, as.Date("2020-03-01"), as.Date("2022-03-01")))|>
  summarize(UPT = sum(UPT, na.rm = TRUE))
print(nyc_subway_march_trips)
## 2,341,451,028 subway ridership fall between March 2020 and March 2022

FINANCIALS <- FINANCIALS |>
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

#5: Table Summarization
library(dplyr)
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
## 6A.Which transit system (agency and mode) had the most UPT in 2022?
financials_summary <- USAGE_AND_FINANCIALS |>
  group_by(`Agency`, `Mode`) |>
    summarize(total_UPT = sum(UPT, na.rm = TRUE))

max_UPT <- max(financials_summary$total_UPT, na.rm = TRUE)

top_transit_system <- financials_summary |>
  filter(total_UPT == max_UPT)

print(top_transit_system)
## Connecticut Department of Transportation - CTTRANSIT New Haven Division(Bus)

## 6B.Which transit system (agency and mode) had the highest farebox recovery, defined as the highest ratio of Total Fares to Expenses?
financials_summary <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(total_farebox_recovery = sum(`Total Fares`, na.rm = TRUE) / sum(Expenses, na.rm = TRUE), .groups= "drop")

max_farebox_recovery <- max(financials_summary$total_farebox_recovery, na.rm = TRUE)

highest_farebox_recovery <- financials_summary |>
  filter(total_farebox_recovery == max_farebox_recovery)

print(highest_farebox_recovery)
##	County of Miami-Dade (Vanpool)

##6C. Which transit system (agency and mode) has the lowest expenses per UPT?
financials_summary <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(expenses_per_UPT = sum(Expenses, na.rm = TRUE) / sum(UPT, na.rm = TRUE), .groups = "drop")

min_expenses_per_UPT <- min(financials_summary$expenses_per_UPT, na.rm = TRUE)

lowest_expense_transit_system <- financials_summary |>
  filter(expenses_per_UPT == min_expenses_per_UPT)

print(lowest_expense_transit_system)
## Enterprise Holdings, LLC (Vanpool)

##6D.Which transit system (agency and mode) has the highest total fares per UPT?
financials_summary <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(total_fares_per_UPT = sum(`Total Fares`, na.rm = TRUE) / sum(UPT, na.rm = TRUE), .groups = "drop")

max_total_fares_per_UPT <- max(financials_summary$total_fares_per_UPT, na.rm = TRUE)

highest_total_fares_transit_system <- financials_summary |>
  filter(total_fares_per_UPT == max_total_fares_per_UPT)

print(highest_total_fares_transit_system)
## Bay State LLC (Ferryboats)

## 6E. Which transit system (agency and mode) has the lowest expenses per VRM?
financials_summary <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(expenses_per_VRM = sum(Expenses, na.rm = TRUE) / sum(VRM, na.rm = TRUE), .groups = "drop")

min_expenses_per_VRM <- min(financials_summary$expenses_per_VRM, na.rm = TRUE)

lowest_expense_transit_system_VRM <- financials_summary |>
  filter(expenses_per_VRM == min_expenses_per_VRM)

print(lowest_expense_transit_system_VRM)
## Enterprise Holdings, LLC (Vanpool)

### 6F.Which transit system (agency and mode) has the highest total fares per VRM?
financials_summary <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarize(total_fares_per_VRM = sum(`Total Fares`, na.rm = TRUE) / sum(VRM, na.rm = TRUE), .groups = "drop")

max_total_fares_per_VRM <- max(financials_summary$total_fares_per_VRM, na.rm = TRUE)

highest_total_fares_transit_system_VRM <- financials_summary |>
  filter(total_fares_per_VRM == max_total_fares_per_VRM)

print(highest_total_fares_transit_system_VRM)
### Hyannis Harbor Tours, Inc. (Ferryboats)
  