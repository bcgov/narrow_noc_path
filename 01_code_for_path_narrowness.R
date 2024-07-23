library(tidyverse)
library(here)
library(vroom)
library(janitor)
library(openxlsx)
library(readxl)
library(plotly)
library(conflicted)
conflicts_prefer(dplyr::filter)
#constants
margin_greater_than <- 1000 #only keep rows and columns that sum to more than this
#functions
fix_data <- function(vctr){
  vctr|>
    str_remove_all(",")|>
    as.numeric()
}
process_data <- function(file){
  raw <- vroom::vroom(here("data", file), skip= 12, n_max = 3481) #garbage before and after data
  colnames(raw)[1] <- "Field of Study" #missing column name
  colnames(raw)[2] <- "Highest attainment"
  raw <- raw[-1,] #garbage in first row
  edu_noc <- raw|>
    remove_empty(c("cols"))|>
    fill(`Field of Study`, .direction = "down")|>
    mutate(across(-c("Field of Study", "Highest attainment"), fix_data))|>
    unite("Education", "Field of Study", "Highest attainment", sep=": ")|>
    adorn_totals("both")
  #here we do some filtering to limit the sparsity of the data---------------
  keep_columns <- as.vector(edu_noc[edu_noc$Education=="Total",] > margin_greater_than)
  edu_noc <- edu_noc[edu_noc$Total>margin_greater_than, keep_columns]|>
    select(-Total)|>
    filter(Education!="Total")|>
    pivot_longer(cols=-Education, names_to = "NOC", values_to = "value")
  #write to disk------------------------------
  write_csv(edu_noc, here("out", file))
  return(edu_noc)
}
path_to <- function(tbbl){
  tbbl|>
    group_by(NOC)|>
    mutate(value=value/sum(value))|>
    filter(value>.01)|>
    mutate(greater_than_one=n())|>
    slice_max(value, n=5, with_ties = FALSE)|>
    mutate(value=round(value, 3))|>
    arrange(desc(greater_than_one), NOC,  desc(value))|>
    separate(Education, into = c("in this field of study","attained this level"), sep=": ")|>
    select(`For workers in this occupation`=NOC,
           `this proportion`=value,
           `attained this level`,
           `in this field of study`,
           `# of educations with share > 1%`=greater_than_one)
}
path_from <- function(tbbl){
  tbbl|>
    group_by(Education)|>
    mutate(value=value/sum(value))|>
    filter(value>.01)|>
    mutate(greater_than_one=n())|>
    slice_max(value, n=5, with_ties = FALSE)|>
    mutate(value=round(value, 3))|>
    arrange(desc(greater_than_one), Education, desc(value))|>
    separate(Education, into = c("in this field of study","For students who attained"), sep=": ")|>
    select(`For students who attained`,
           `in this field of study`,
           `this proportion`=value,
           `ended up in this occupation`=NOC,
           `# of educations with share > 1%`=greater_than_one)
}
# get and process the data
edu_noc_all <- process_data("cip_noc_all_ages.csv")
edu_noc_25_64 <- process_data("cip_noc_25-64.csv")
# get the paths to and from
edu_noc_all_to <- path_to(edu_noc_all)
edu_noc_all_from <- path_from(edu_noc_all)
edu_noc_25_64_to <- path_to(edu_noc_25_64)
edu_noc_25_64_from <- path_from(edu_noc_25_64)
## Create a new workbook
wb <- createWorkbook()
#header formatting
hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 15,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)
# Add a worksheet
addWorksheet(wb, "Paths to Occupation all ages")
addWorksheet(wb, "Paths from Education all ages")
addWorksheet(wb, "Paths to Occupation ages 25-64")
addWorksheet(wb, "Paths from Education ages 25-64")
#write the data
writeData(wb, sheet = "Paths to Occupation all ages", x = edu_noc_all_to, headerStyle = hs, withFilter = TRUE)
writeData(wb, sheet = "Paths from Education all ages", x = edu_noc_all_from, headerStyle = hs, withFilter = TRUE)
writeData(wb, sheet = "Paths to Occupation ages 25-64", x = edu_noc_25_64_to, headerStyle = hs, withFilter = TRUE)
writeData(wb, sheet = "Paths from Education ages 25-64", x = edu_noc_25_64_from, headerStyle = hs, withFilter = TRUE)
## set col widths
setColWidths(wb, "Paths to Occupation all ages", cols = 1:5, widths = c(70,25,55,70,30))
setColWidths(wb, "Paths from Education all ages", cols = 1:5, widths = c(55,70,25,70,30))
setColWidths(wb, "Paths to Occupation ages 25-64", cols = 1:5, widths = c(70,25,55,70,30))
setColWidths(wb, "Paths from Education ages 25-64", cols = 1:5, widths = c(55,70,25,70,30))
## Save workbook
saveWorkbook(wb, here("out", "top_paths_to_and_from.xlsx"), overwrite = TRUE)

#investigate age filtering differences:

edu_noc_to <- inner_join(edu_noc_all_to,
                        edu_noc_25_64_to, by = join_by(`For workers in this occupation`,
                                                       `attained this level`,
                                                       `in this field of study`))
with(edu_noc_to, cor(`this proportion.x`,
                     `this proportion.y`))

with(edu_noc_to, cor(`# of educations with share > 1%.x`,
                     `# of educations with share > 1%.y`))


edu_noc_from <- inner_join(edu_noc_all_from,
                        edu_noc_25_64_from,
                        by = join_by(`For students who attained`,
                                     `in this field of study`,
                                     `ended up in this occupation`))

with(edu_noc_from, cor(`this proportion.x`,
                     `this proportion.y`))


with(edu_noc_from, cor(`# of educations with share > 1%.x`,
                     `# of educations with share > 1%.y`))


