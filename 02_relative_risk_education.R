library(tidyverse)
library(here)
library(readxl)
library(plotly)
library(conflicted)
conflicts_prefer(dplyr::filter)

#constants------------
cut_off=.2
#functions---------------
get_relative_risk <- function(edu, tbbl){
  prob_bad_conditional_on_education <- tbbl|>
    filter(Education==edu)|>
    group_by(occ_type)|>
    summarize(value=sum(value))|>
    ungroup()|>
    mutate(value=value/sum(value))|>
    filter(occ_type=="bad")|>
    pull(value)

  prob_bad_conditional_on_not_education <- tbbl|>
    filter(Education!=edu)|>
    group_by(occ_type)|>
    summarize(value=sum(value))|>
    ungroup()|>
    mutate(value=value/sum(value))|>
    filter(occ_type=="bad")|>
    pull(value)

  relative_risk <- prob_bad_conditional_on_education/prob_bad_conditional_on_not_education
}

#the code--------------------
cip_noc_all <- read_csv(here("out","cip_noc_all_ages_processed.csv"))|>
  rename(NOCtemp=NOC)|>
  mutate(NOC=str_sub(NOCtemp, 1,5),
         Description=str_sub(NOCtemp, 7), .before=everything())|>
  select(-NOCtemp)

noc_names <- cip_noc_all|>
  select(NOC, Description)|>
  unique()

median_salaries <- read_excel(here("data","median_salary.xlsx"))|>
  select(NOC, median_salary=contains("calculated"))

paths <- read_excel(here("out","top_paths_to_and_from.xlsx"), sheet = "Paths from Education all ages")|>
  select(highest=`For students who attained`,CIP=`in this field of study`, paths=`# of educations with share > 1%`)|>
  distinct()

skills <- read_excel(here("data", "skills_original.xlsx"))|>
  mutate(NOC=str_pad(noc2021, 5, "left", "0"), .before=everything())|>
  select(-noc2021)|>
  group_by(NOC)|>
  summarize(average_skill=round(mean(value_mean),2))

low_median_salary <- median_salaries|>
  slice_min(median_salary, prop = cut_off)

low_skills <- skills|>
  slice_min(average_skill, prop = cut_off)

bad_occ <- inner_join(low_median_salary, low_skills)|>
  mutate(occ_type="bad")|>
  left_join(noc_names)

joined <- full_join(cip_noc_all, bad_occ)|>
  mutate(occ_type=if_else(is.na(occ_type), "good",  occ_type))

nested <- tibble(Education=unique(joined$Education), data=list(joined))|>
  na.omit()|>
  mutate(relative_risk=map2_dbl(Education, data, get_relative_risk))

relative_risk <- nested|>
  select(Education, relative_risk)|>
  separate(Education, into=c("CIP","highest"), sep=": ")

write_csv(bad_occ, here("out","bad_occ.csv"))
write_csv(relative_risk, here("out","relative_risk.csv"))
