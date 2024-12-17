library(tidyverse)
library(here)
library(readxl)
library(plotly)
library(conflicted)
conflicts_prefer(dplyr::filter)

#constants------------
cut_off=.2
#functions---------------
get_relative_risk <- function(edu){
  prob_hshs_conditional_on_education <- joined|>
    filter(Education==edu)|>
    group_by(occ_type)|>
    summarize(value=sum(value))|>
    ungroup()|>
    mutate(value=value/sum(value))|>
    filter(occ_type=="HSHS")|>
    pull(value)

  prob_hshs_conditional_on_not_education <- joined|>
    filter(Education!=edu)|>
    group_by(occ_type)|>
    summarize(value=sum(value))|>
    ungroup()|>
    mutate(value=value/sum(value))|>
    filter(occ_type=="HSHS")|>
    pull(value)

  relative_risk <- prob_hshs_conditional_on_education/prob_hshs_conditional_on_not_education
}

#the code--------------------
cip_noc_all <- read_csv(here("out","cip_noc_all_ages.csv"))|>
  rename(NOCtemp=NOC)|>
  mutate(NOC=str_sub(NOCtemp, 1,5),
         Description=str_sub(NOCtemp, 7), .before=everything())|>
  select(-NOCtemp)

noc_names <- cip_noc_all|>
  select(NOC, Description)|>
  unique()

median_salaries <- read_excel(here("data","median_salary.xlsx"))|>
  select(NOC, median_salary=contains("calculated"))

skills <- read_excel(here("data", "skills_original.xlsx"))|>
  mutate(NOC=str_pad(noc2021, 5, "left", "0"), .before=everything())|>
  select(-noc2021)|>
  group_by(NOC)|>
  summarize(average_skill=round(mean(value_mean),2))

high_median_salary <- median_salaries|>
  slice_max(median_salary, prop = cut_off)

high_skills <- skills|>
  slice_max(average_skill, prop = cut_off)

hshs_occ <- inner_join(high_median_salary, high_skills)|>
  mutate(occ_type="HSHS")|>
  left_join(noc_names)

joined <- full_join(cip_noc_all, hshs_occ)|>
  mutate(occ_type=if_else(is.na(occ_type), "not",  occ_type))

nested <- tibble(Education=unique(joined$Education))|>
  na.omit()|>
  mutate(relative_risk=map_dbl(Education, get_relative_risk))

relative_risk <- nested|>
  select(Education, relative_risk)|>
  separate(Education, into=c("CIP","highest"), sep=": ")

write_csv(hshs_occ, here("out","hshs_occ.csv"))
write_csv(relative_risk, here("out","relative_risk_hshs.csv"))

