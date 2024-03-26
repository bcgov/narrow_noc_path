library(tidyverse)
library(here)
library(vroom)
library(janitor)
library(openxlsx)

#constants
margin_greater_than <- 1000 #only keep rows and columns that sum to more than this

fix_data <- function(vctr){
  vctr|>
    str_remove_all(",")|>
    as.numeric()
}

raw <- vroom::vroom(here("data", "raw_cip_noc_attain.csv"), skip= 12, n_max = 3481) #garbage before and after data
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
keep_columns <- as.vector(edu_noc[edu_noc$Education=="Total",]>margin_greater_than)
edu_noc <- edu_noc[edu_noc$Total>margin_greater_than, keep_columns]|>
  select(-Total)|>
  filter(Education!="Total")|>
  pivot_longer(cols=-Education, names_to = "NOC", values_to = "value")

#write to disk------------------------------
write_csv(edu_noc, here("out","edu_noc.csv"))

path_to <- edu_noc|>
  group_by(NOC)|>
  mutate(value=value/sum(value))|>
  slice_max(value, n=1)|>
  arrange(desc(value))|>
  mutate(value=round(value,3))|>
  separate(Education, into = c("in this field of study","attained this level"), sep=": ")|>
  select(`For workers in this occupation`=NOC,
         `this proportion`=value,
         `attained this level`,
          `in this field of study`)

#do same by Education

path_from <- edu_noc|>
  group_by(Education)|>
  mutate(value=value/sum(value))|>
  slice_max(value, n=1)|>
  arrange(desc(value))|>
  mutate(value=round(value,3))|>
  separate(Education, into = c("in this field of study","For students who attained"), sep=": ")|>
  select(`For students who attained`,
         `in this field of study`,
         `this proportion`=value,
         `ended up in this occupation`=NOC)

## Create a new workbook
wb <- createWorkbook()

hs <- createStyle(
  textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 15,
  fontName = "Arial Narrow", fgFill = "#4F80BD"
)

## Add a worksheet
addWorksheet(wb, "Paths to Occupation")
addWorksheet(wb, "Paths from Education")

writeData(wb, sheet = "Paths to Occupation", x = path_to, headerStyle = hs, withFilter = TRUE)
writeData(wb, sheet = "Paths from Education", x = path_from, headerStyle = hs, withFilter = TRUE)

## set col widths
setColWidths(wb, "Paths to Occupation", cols = 1:4, widths = c(70,15,55,70))
setColWidths(wb, "Paths from Education", cols = 1:4, widths = c(55,70,15,70))

## Save workbook

saveWorkbook(wb, here("out", "top_paths_to_and_from.xlsx"), overwrite = TRUE)




