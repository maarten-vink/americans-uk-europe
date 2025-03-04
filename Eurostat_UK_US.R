# Code to replicate plots posted in a Bluesky thread on naturalisations of Americans in UK and rest of Europe
# Maarten Vink, EUI | @maartenpvink
# https://bsky.app/profile/maartenpvink.bsky.social/post/3ljkcae7qgc24

#start with clean workspace
rm(list=ls(all=TRUE))

library(tidyverse)
library(eurostat) # see also: https://ropengov.github.io/eurostat/articles/eurostat_tutorial.html
library(did)
library(lubridate)
library(ggthemes)

#make variable for 27 EU member states + 3 associated countries 
EU27plus <- c("AT","BE","BG", "CY", "CZ","DK","EE", "FI","FR","DE","EL","HU", "HR", "IE","IT",
              "LV","LU", "LT","MT","NL","PL","PT","RO", "SK", "SI","ES","SE",
              "CH", "IS","NO")
EU27plus

EU27 <- c("AT","BE","BG", "CY", "CZ","DK","EE", "FI","FR","DE","EL","HU", "HR", "IE","IT",
          "LV","LU", "LT","MT","NL","PL","PT","RO", "SK", "SI","ES","SE")

EU28 <- c("AT","BE","BG", "CY", "CZ","DK","EE", "FI","FR","DE","EL","HU", "HR", "IE","IT",
              "LV","LU", "LT","MT","NL","PL","PT","RO", "SK", "SI","ES","SE", "UK")

# download data on citizenship acquisitions from Eurostat
dat_acq <- get_eurostat("migr_acq")
# Export dataset
# write.csv(dat_acq, file = paste0("Eurostat_cit_data_", Sys.Date(), ".csv"), row.names = FALSE)
# dat_acq <- read.csv("Eurostat_cit_data_2025-02-28.csv", stringsAsFactors = FALSE) 

# select data
d1 <- dat_acq |>
  filter(geo %in% EU28) |>
  filter(age == "TOTAL") |>
  filter(agedef == "COMPLET") |>
  filter(unit == "NR") |>
  filter(sex == "T") |>
  filter(citizen == "US") |>
  mutate(year = as.numeric(format(as.Date(TIME_PERIOD), format="%Y"))) |>
  filter(year > 2003) |> # 8 years before 2016, 8 years from 2016 (2023)
  mutate(eu27 = ifelse(geo %in% EU27, "EU27", "UK")) |>
  select(geo, year, cit_acq = values, citizen, eu27) 
View(d1) #505 obs of 5 vars


#plot with totals
d2 <- d1 |>
  group_by(year, eu27) |> 
  summarise(total = sum(cit_acq))
View(d2)

#plot
fig1 <- d2 |> 
  filter(eu27 == "EU27") |>
  ggplot(aes(x=year, y=total)) + 
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_x_continuous(breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022))+
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
 # geom_text(aes(label = total), vjust = -0.2, colour = "black")+
  labs(title = "Citizenship acquisitions by Americans in EU27, total, 2004 - 2023",
       subtitle = "\nStatistics refer to the usually resident population (so exclude citizenship acquisition from abroad due to special ties) ",
       caption = "Data source: Eurostat [migr_acq], 27-02-2025 | @maartenpvink.bsky.social")
fig1

#Save as jpeg file
jpeg("Fig_citaq_Americans_EU27.jpeg", width=12, height=8, units="in", res=200)
fig1
dev.off()


#make plot for selected countries

#select data on UK nationals cit acq in European countries 
#calculate top3 post-brexit citizenship granting countries by citizenship granting country
d3 <- d1 |>
  group_by(geo) |> 
  summarise(total = sum(cit_acq)) |>
  arrange(desc(total))
View(d3)


#plot trend by countries
fig2 <- d1 |> 
  ggplot(aes(x=year, y=cit_acq, fill = eu27)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~geo, scales = "free")+
  theme_economist() +
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  labs(title = "Citizenship acquisitions by Americans in EU27 countries and UK, 2004 - 2023",
       subtitle = "\nEurostat data for UK only up to 2019",
       caption = "Data source: Eurostat [migr_acq], 27-02-2025 | @maartenpvink.bsky.social")
fig2

#Save as jpeg file
jpeg("Fig_citaq_Americans_EU27_UK.jpeg", width=12, height=8, units="in", res=500)
fig2
dev.off()


### explore with OECD data
# import OECD data on inflows and stocks of US citizens into European destinations and citizenship acquisitions 
# https://www.oecd.org/en/data/datasets/overview-data-on-migration-flows-and-migrant-populations.html

#inflows
inflows <- read.csv("OECD_Europe_US_inflows.csv") |>
  mutate(iso3 = REF_AREA) |>
  mutate(country = Reference.area) |>
  mutate(year = TIME_PERIOD) |>
  mutate(inflows = OBS_VALUE) |>
  select(iso3, country, year, inflows) 
summary(inflows) # 428 obs of 4 vars #No UK data available!!

# import OECD data on stock of foreign population
stock <- read.csv("OECD_Europe_US_stock.csv") |>
  mutate(iso3 = REF_AREA) |>
  mutate(country = Reference.area) |>
  mutate(year = TIME_PERIOD) |>
  mutate(stock = OBS_VALUE) |>
  select(iso3, country, year, stock) 
summary(stock) # 417 obs of 4 vars

# cit acq
citacq <- read.csv("OECD_Europe_US_citacq.csv") |>
  mutate(iso3 = REF_AREA) |>
  mutate(country = Reference.area) |>
  mutate(year = TIME_PERIOD) |>
  mutate(citacq = OBS_VALUE) |>
  select(iso3, country, year, citacq) 
summary(citacq) # 422 obs of 4 vars

#plot with totals

# code variable for rest of europe v uk
roe <- c("AUT","BEL","CZE","DNK","EST", "FIN","FRA","DEU","GRC","HUN", "ISL", "IRL","ITA",
         "LVA","LUX", "LTU","NLD","NOR","POL","PRT","SVK", "SVN","ESP","SWE", "CHE")

# overview plot stocks
stock_totals <- stock |>
  mutate(roe_uk = ifelse(iso3 %in% roe, "Rest of Europe", "UK")) |>
  group_by(year, roe_uk) |> 
  summarise(total = sum(stock)/1000) |>
  arrange(desc(total))
View(stock_totals)

#plot
fig_stock <- stock_totals |> 
  ggplot(aes(x=year, y=total)) +
  facet_wrap(~roe_uk) +
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_x_continuous(breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022))+
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  # geom_text(aes(label = total), vjust = -0.2, colour = "black")+
  labs(title = "Stock of US citizens residing in the UK and the rest of Europe, 2004 - 2022 (in 000s)",
       subtitle = "\nData missing for various years",
       caption = "Data source: OECD IMD (inflows), 28-02-2025 | @maartenpvink.bsky.social")
fig_stock

#Save as jpeg file
jpeg("Fig_stock_Americans_uk_roe.jpeg", width=12, height=8, units="in", res=200)
fig_stock
dev.off()

# overview citaqc flows
citacq_totals <- citacq |>
  mutate(roe_uk = ifelse(iso3 %in% roe, "Rest of Europe", "UK")) |>
  group_by(year, roe_uk) |> 
  summarise(total = sum(citacq)/1000) |>
  arrange(desc(total))
View(citacq_totals)

# save data OECD
require(openxlsx)
list_of_datasets <- list("Eurostat_US in EU28" = d1, "Eurostat_total_year_EU27_UK" = d2,
                         "Eurostat_total_country_trend" = d3,
                         "OECD_stock_US_UK_RoE" = stock, "OECD_stock_US_in_UK_RoE_totals" = stock_totals,
                         "OECD_citacq_US_UK_RoE" = citacq, "OECD_citacq_US_in_UK_RoE_totals" = citacq_totals)
write.xlsx(list_of_datasets, file = "Eurostat_OECD_UK_US.xlsx")


#plot
fig_citacq <- citacq_totals |> 
  ggplot(aes(x=year, y=total)) +
  facet_wrap(~roe_uk) +
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_x_continuous(breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022))+
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  # geom_text(aes(label = total), vjust = -0.2, colour = "black")+
  labs(title = "Citizenship acquisition by US citizens in the UK and the rest of Europe, 2004 - 2022 (in 000s)",
       subtitle = "\nPeak in 2012-13 in RoE: 3576 acquisitions in Austria, 5085 in Germany (mostly by Americans residing abroad)\n",
       caption = "Data source: OECD IMD (citizenship acquisition), 28-02-2025 (missing for 2012-13 in RoE) | @maartenpvink.bsky.social")
fig_citacq

#Save as jpeg file
jpeg("Fig_citacq_Americans_uk_roe.jpeg", width=12, height=8, units="in", res=200)
fig_citacq
dev.off()

# us UK statistics on settlement
# https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#settlement
stock_UK_US <- read.csv("settlement-datasets-dec-2024.csv") |>
  filter(Nationality == 'United States') |>
  filter(Case.Outcome == 'Grant') |>
  mutate(decisions = as.numeric(Decisions)) |>
  mutate(year = as.numeric(Year)) |>
  select(year, decisions)
summary(stock_UK_US) # 692 obs of 9 vars

class(stock_UK_US$year)
#totals
stock_UK_US_total <- stock_UK_US |>
  na.omit() |>
  group_by(year) |>
  mutate(total = sum(decisions)) |>
  select(year, total) |>
  distinct()
stock_UK_US_total

#plot
fig_stock_UK_US <- stock_UK_US_total |> 
  ggplot(aes(x=year, y=total)) +
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_x_continuous(breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024))+
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  # geom_text(aes(label = total), vjust = -0.2, colour = "black")+
  labs(title = "Grants of settlement to US citizens in the UK, 2004 to 2023",
       subtitle = "\n",
       caption = "Data source: Home Office (Settlement detailed datasets, year ending December 2024), 28-02-2025 | @maartenpvink.bsky.social")
fig_stock_UK_US

#Save as jpeg file
jpeg("Fig_stock_Americans_UK.jpeg", width=12, height=8, units="in", res=200)
fig_stock_UK_US
dev.off()

# us UK statistics on cit applications
# https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#settlement
citapps_UK_US <- read.csv("citizenship-datasets-dec-2024-apps.csv") |>
  filter(Nationality == 'United States') |>
  mutate(type = as.factor(Application.type.group)) |>
  mutate(applications = as.numeric(Applications)) |>
  mutate(year = as.numeric(Year)) |>
  select(year, type, applications)
summary(citapps_UK_US) # 692 obs of 9 vars

#totals
citapps_UK_US_total <- citapps_UK_US |>
  group_by(year, type) |>
  mutate(total = sum(applications)) |>
  select(year, total) |>
  distinct()
citapps_UK_US_total

#plot
fig_citapps_UK_US <- citapps_UK_US_total |> 
  ggplot(aes(x=year, y=total, fill = type)) +
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_x_continuous(breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024))+
  ylab("")+
  xlab("")+
  theme(legend.position = "top")+
  theme(plot.title = element_text(face = "bold"))+
  # geom_text(aes(label = total), vjust = -0.2, colour = "black")+
  labs(title = "Applications for British citizenship by US citizens in the UK, by type, 2005 to 2024",
       subtitle = "\n",
       caption = "Data source: Home Office (Citizenship detailed datasets, year ending December 2024), 28-02-2025 | @maartenpvink.bsky.social")
fig_citapps_UK_US

#Save as jpeg file
jpeg("Fig_citapps_Americans_UK.jpeg", width=12, height=8, units="in", res=200)
fig_citapps_UK_US
dev.off()


# us UK statistics on cit acqs
# https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#settlement
citacq_UK_US <- read.csv("citizenship-datasets-dec-2024.csv") |>
  filter(Nationality == 'United States') |>
  mutate(grants = as.numeric(Grants)) |>
  mutate(year = as.numeric(Year)) |>
  select(year, grants) 
summary(citacq_UK_US) # 692 obs of 9 vars

#totals
citacq_UK_US_total <- citacq_UK_US |>
  na.omit() |>
  group_by(year) |>
  mutate(total = sum(grants)) |>
  select(year, total) |>
  distinct()
citacq_UK_US_total

# save data OECD
require(openxlsx)
list_of_datasets <- list("Eurostat_US_EU28" = d1, "Eurostat_total_year_EU27_UK" = d2,
                         "Eurostat_total_country_trend" = d3,
                         "OECD_stock_US_UK_RoE" = stock, "OECD_stock_US_in_UK_RoE_totals" = stock_totals,
                         "OECD_citacq_US_UK_RoE" = citacq, "OECD_citacq_US_in_UK_RoE_totals" = citacq_totals,
                         "UK_settlement_US" = stock_UK_US, "UK_settlement_US_totals" = stock_UK_US_total,
                         "UK_citapps_US" = citapps_UK_US, "UK_citapps_US_totals" = citapps_UK_US_total,
                         "UK_citacq_US" = citacq_UK_US, "UK_citacqs_US_totals" = citacq_UK_US_total)
write.xlsx(list_of_datasets, file = "Eurostat_OECD_UK_US.xlsx")



#plot
fig_citacq_UK_US <- citacq_UK_US_total |> 
  ggplot(aes(x=year, y=total)) +
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_x_continuous(breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024))+
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  # geom_text(aes(label = total), vjust = -0.2, colour = "black")+
  labs(title = "Grants of British citizenship to US citizens in the UK, 2005 to 2024",
       subtitle = "\n",
       caption = "Data source: Home Office (Citizenship detailed datasets, year ending December 2024), 28-02-2025 | @maartenpvink.bsky.social")
fig_citacq_UK_US

#Save as jpeg file
jpeg("Fig_citacq_Americans_UK.jpeg", width=12, height=8, units="in", res=200)
fig_citacq_UK_US
dev.off()
#End
