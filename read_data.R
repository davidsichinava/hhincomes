library(tidyverse)
library(haven)
library(extrafont)


setwd("D:\\Dropbox (Personal)\\My projects\\scratchpad\\hhinc")

###### Set up theme

loadfonts(device = "win")

theme_ef <- function () { 
  theme_minimal(base_size=12) %+replace%
    theme(
      axis.title = element_blank(),
      title = element_blank(),
      # axis.text.x = element_blank(),
      # panel.grid = element_blank(),
      strip.text = element_text(family = "Lato", face="bold", size = 14),
      text = element_text(family= "Lato"),
      plot.title = element_text(size=14, face="bold", family="Lato", hjust = 0),
      plot.subtitle = element_text(size=12, family="Lato", hjust=0),
      axis.text = element_text(size=12, family="Lato", color = "black"),
      legend.position = "none"
    )
}

##### Set up Shindas
###  2002-2015

years <- rep(2002:2015)

shinda <- data.frame()

for(i in seq_along(years)) {
  tblincomes <- read_sav(unz(description= "datasets/Datasets.zip",
                             filename = paste0(years[i], ' DB/tblincomes.sav')))
  SysSchedule <- read_sav(unz(description= "datasets/Datasets.zip",
                             filename = paste0(years[i], ' DB/Sysschedule.sav')))
  SysSchedule |>
    left_join(tblincomes, by = "UID") |>
    mutate(
      year = years[i]
    ) |> 
    bind_rows(shinda) -> shinda
}

tblincomes <- read_sav(unz(description= "datasets/Datasets.zip",
                           filename = paste0(2016, ' DB/tblincomes.sav')))
SysSchedule <- read_sav(unz(description= "datasets/Datasets.zip",
                            filename = paste0(2016, ' DB/sysschedule.sav')))

SysSchedule |>
  left_join(tblincomes, by = "UID") |>
  mutate(
    year = 2016
  ) |> 
  bind_rows(shinda) -> shinda

tblincomes <- read_sav(unz(description= "datasets/Datasets.zip",
                           filename = '2017 db/2017 DB/tblincomes.sav'))


SysSchedule <- read_sav(unz(description= "datasets/Datasets.zip",
                            filename = '2017 db/2017 DB/sysschedule.sav'))

SysSchedule |>
  left_join(tblincomes, by = "UID") |>
  mutate(
    year = 2017
  ) |> 
  bind_rows(shinda) -> shinda


years <- rep(2018:2020)

for(i in seq_along(years)) {
  tblincomes <- read_sav(unz(description= paste0("datasets/", years[i], "-DB.zip"),
                             filename = paste0(years[i], ' DB/tblincomes.sav')))
  SysSchedule <- read_sav(unz(description= paste0("datasets/", years[i], "-DB.zip"),
                              filename = paste0(years[i], ' DB/sysschedule.sav')))
  SysSchedule |>
    left_join(tblincomes, by = "UID") |>
    mutate(
      year = years[i]
    ) |> 
    bind_rows(shinda) -> shinda
}

tblincomes_21 <- read_sav(unz(description= "datasets/2021-DB.zip",
                           filename = paste0('GEO/', 2021, ' DB/tblincomes.sav')))
SysSchedule_21 <- read_sav(unz(description= "datasets/2021-DB.zip",
                            filename = paste0('GEO/',2021, ' DB/sysschedule.sav')))
tblshinda01_21 <- read_sav(unz(description= "datasets/2021-DB.zip",
                            filename = paste0('GEO/',2021, ' DB/tblshinda01.sav')))

tblincomes_22 <- read_sav(unz(description= "datasets/2022-DB.zip",
                           filename = paste0('GEO/', 2022, ' DB/tblincomes.sav')))
SysSchedule_22 <- read_sav(unz(description= "datasets/2022-DB.zip",
                            filename = paste0('GEO/',2022, ' DB/sysschedule.sav')))
tblshinda01_22 <- read_sav(unz(description= "datasets/2022-DB.zip",
                            filename = paste0('GEO/',2022, ' DB/tblshinda01.sav')))

SysSchedule_21 |>
  left_join(tblincomes_21, by = "UID") |>
  mutate(
    year = 2021
  ) -> shinda_21

SysSchedule_22 |>
  left_join(tblincomes_22, by = "UID") |>
  mutate(
    year = 2022
  ) -> shinda_22

shinda_21 |> 
  bind_rows(shinda_22, shinda) -> shinda


shinda_22 |> 
  # filter(year == 2021) |>
  left_join(tblshinda01_22, by = "UID") -> shinda_2022


# |> 
#   bind_rows(shinda) -> shinda


shinda_2022 |>   
  mutate(
    OwnerOfHome = as_factor(OwnerOfHome)
  ) |> 
  group_by(RegNo, OwnerOfHome) |> 
  summarize(
      income_med = median(Shemosavalisul, wt = Weights, na.rm = T),
      income_mean = mean(Shemosavalisul, wt = Weights, na.rm = T),
    )




#### Read and process CPI

# readxl::read_xls(path = "cpi.xls", sheet = "SaqarTvelo")

readxl::read_excel("samomxmareblo-fasebis-indeqsebi-2010-wlis-sashualo=100.xlsx", 1) |> 
  slice(3:n()) |> 
  set_names("year", paste0("m", 1:12)) |> 
  pivot_longer(-year, names_to = "month", values_to = "cpi") |> 
  mutate(
    cpi = as.numeric(cpi)
  ) |> 
  group_by(year) |> 
  summarize(
    cpi = mean(cpi, na.rm=T)/100
  ) -> cpi


shinda |> 
  group_by(year) |> 
  summarize(
    mean_income = weighted.mean(Shemosavalisul, na.rm = T, wt = Weights),
    median_income = matrixStats::weightedMedian(Shemosavalisul, na.rm = T, Weights),
  ) |> 
  left_join(
    cpi, by = "year"
  ) |> 
  mutate(
    income_mean_infl = mean_income/cpi,
    income_median_infl = median_income/cpi,
  ) |>
  select(-cpi) |> 
  pivot_longer(-year, names_to = "category", values_to = "income") |> 
  mutate(
    # case_when(
    #   category == "income_mean_infl" ~ "Inflation-adjusted (2010 prices) mean household income",
    #   T ~ "Inflation-adjusted (2010 prices) median household income",
    # )
    measure = case_when(
      str_detect(category, "mean") ~ "Mean",
      T ~ "Median"
    ),
    type = case_when(
      str_detect(category, "infl") ~ "Adjusted to inflation (base: 2010 prices)",
      T ~ "Nominal values"
    )
  ) |>
  # select(-category) |> 
  # pivot_wider(values_from = "income", names_from = "type") |> 
  # write_csv("mean_median.csv") 
  ggplot(
    aes(year, income, group = type, color = type)
  )+
  geom_line()+
  geom_point()+
  facet_wrap(~measure)+
  scale_color_manual(values = c("#d7263d", "#1b998b"))+
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, 1500))+
  scale_x_continuous(breaks = rep(2002:2022, by = 2))+
  geom_vline(xintercept=2003,
             color="grey", linetype = "longdash")+
  geom_vline(xintercept=2012,
             color="grey", linetype = "longdash")+
  labs(
    title = toupper("Average and median household income in Georgia, 2002-2022 (GEL)"),
    subtitle = "Nominal and inflation-adjusted values in 2010 prices",
    caption = "Own calculations based on Integrated Household Survey (IHS, 2002-2016) and Incomes and Expenditures Survey (IES, 2017-2022) data by Geostat",
    y = "GEL"
  )+
  theme_ef()+
  theme(
    axis.text = element_text(size = 8, angle = 90),
    axis.title.y = element_text(),
    plot.caption = element_text(size = 6, family = "FiraGO"),
    legend.position = "bottom"
  )

ggsave("incomes_nat.pdf", width = 12, height = 6, device = cairo_pdf) 

shinda |> 
  group_by(year, RegNo) |> 
  summarize(
    mean_income = weighted.mean(Shemosavalisul, na.rm = T, wt = Weights),
    median_income = matrixStats::weightedMedian(Shemosavalisul, na.rm = T, Weights),
  ) |> 
  left_join(
    cpi, by = "year"
  ) |> 
  mutate(
    income_mean_infl = mean_income/cpi,
    income_median_infl = median_income/cpi,
  ) |>
  select(-cpi) |> 
  pivot_longer(-c("year","RegNo"), names_to = "category", values_to = "income") |> 
  mutate(
    measure = case_when(
      str_detect(category, "mean") ~ "Mean",
      T ~ "Median"
    ),
    type = case_when(
      str_detect(category, "infl") ~ "Adjusted to inflation (base: 2010 prices)",
      T ~ "Nominal values"
    ),
    RegNo = as_factor(RegNo)
  ) |>
#  filter(measure == "Median" & year == 2021) |> 
  filter(measure == "Median") |> 
  ggplot(
    aes(year, income, group = type, color = type)
  )+
  geom_line()+
  geom_point()+
  facet_wrap(~RegNo, labeller = label_wrap_gen())+
  scale_color_manual(values = c("#d7263d", "#1b998b"))+
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, 1500))+
  scale_x_continuous(breaks = rep(2002:2022, by = 2))+
  geom_vline(xintercept=2003,
             color="grey", linetype = "longdash")+
  geom_vline(xintercept=2012,
             color="grey", linetype = "longdash")+
  labs(
    title = toupper("Median household income in the regions of Georgia, 2002-2022 (GEL)"),
    subtitle = "Nominal and inflation-adjusted values in 2010 prices",
    caption = "Own calculations based on Integrated Household Survey (IHS, 2002-2016) and Incomes and Expenditures Survey (IES, 2017-2022) data by Geostat. In the IHS data, Racha-Lechkhumi and Kvemo Svaneti was grouped with Imereti due to a small population size",
    y = "GEL"
  )+
  theme_ef()+
  theme(
    axis.text = element_text(size = 8, angle = 90),
    axis.title.y = element_text(),
    plot.caption = element_text(size = 6, family = "FiraGO"),
    legend.position = "bottom"
  )

ggsave("incomes_reg.pdf", width = 12, height = 8, device = cairo_pdf) 


shinda |> 
  group_by(year, RegNo) |> 
  summarize(
    mean_income = weighted.mean(Shemosavalisul, na.rm = T, wt = Weights),
    median_income = matrixStats::weightedMedian(Shemosavalisul, na.rm = T, Weights),
  ) |> 
  left_join(
    cpi, by = "year"
  ) |> 
  mutate(
    income_mean_infl = mean_income/cpi,
    income_median_infl = median_income/cpi,
  ) |>
  select(-cpi) |> 
  pivot_longer(-c("year","RegNo"), names_to = "category", values_to = "income") |> 
  mutate(
    measure = case_when(
      str_detect(category, "mean") ~ "Mean",
      T ~ "Median"
    ),
    type = case_when(
      str_detect(category, "infl") ~ "Adjusted to inflation (base: 2010 prices)",
      T ~ "Nominal values"
    ),
    RegNo = as_factor(RegNo)
  ) |>
  # filter(year >= 2021) |> 
  write_csv("incomes_mean_median_region.csv")

shinda |> 
  group_by(year) |> 
  summarize(
    mean_income = weighted.mean(Shemosavalisul, na.rm = T, wt = Weights),
    median_income = matrixStats::weightedMedian(Shemosavalisul, na.rm = T, Weights),
  ) |> 
  left_join(
    cpi, by = "year"
  ) |> 
  mutate(
    income_mean_infl = mean_income/cpi,
    income_median_infl = median_income/cpi,
  ) |>
  select(-cpi) |> 
  pivot_longer(-year, names_to = "category", values_to = "income") |> 
  mutate(
    # case_when(
    #   category == "income_mean_infl" ~ "Inflation-adjusted (2010 prices) mean household income",
    #   T ~ "Inflation-adjusted (2010 prices) median household income",
    # )
    measure = case_when(
      str_detect(category, "mean") ~ "Mean",
      T ~ "Median"
    ),
    type = case_when(
      str_detect(category, "infl") ~ "Adjusted to inflation (base: 2010 prices)",
      T ~ "Nominal values"
    )
  ) |>
  select(-category) |>
  # filter(year >= 2021) |> 
  pivot_wider(values_from = "income", names_from = "type") |>
  # arrange(desc(`Adjusted to inflation (base: 2010 prices)`)) |> 
  # arrange(desc(`Nominal values`)) |> 
  mutate(
    `Nominal values` = round(`Nominal values`, 0),
    `Adjusted to inflation (base: 2010 prices)` = round(`Adjusted to inflation (base: 2010 prices)`, 0),
  ) |> 
  write.csv("incomes_mean_median_national.csv")
