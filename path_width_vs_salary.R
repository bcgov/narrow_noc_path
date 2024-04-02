library(tidyverse)
library(here)
library(readxl)

salaries <- read_rds(here("out", "joined.rds"))|>
  unnest(data)|>
  select(-salary_plot,-weighted_sd, -lower,-upper)

paths <- read_excel(here("out","top_paths_to_and_from.xlsx"), sheet = "Paths from Education")|>
  rename(top_destination=`ended up in this occupation`)

joined <- inner_join(salaries,
                     paths,
                     by=join_by("Highest"=="For students who attained",
                                "CIP"=="in this field of study"))|>
  mutate(Highest=factor(Highest,
                        ordered = TRUE,
                        levels=c("Apprenticeship or trades certificate or diploma",
                                 "College, CEGEP or other non-university certificate or diploma 4",
                                 "University certificate or diploma below bachelor level",
                                 "Bachelor's degree",
                                 "University certificate or diploma above bachelor level",
                                 "Master's degree",
                                 "Earned doctorate 5",
                                 "Degree in medicine, dentistry, veterinary medicine or optometry")))

plt <- ggplot(joined, aes(`this proportion`,
                          weighted_mean))+
  geom_smooth(method="lm", se=FALSE, colour="grey", lwd=1)+
  geom_point(aes(colour=Highest,
                 text=paste0(
                   "Field of Study: ",
                   CIP,
                   "\n Highest Attained: ",
                   Highest,
                   "\n Narrowness of Path = ",
                   `this proportion`,
                   "\n Weighted average median salary = ",
                   scales::dollar(weighted_mean),
                   "\n Top occupation: ",
                   top_destination
                 )))+
  scale_y_continuous(trans="log10", labels = scales::dollar)+
  scale_x_continuous(trans="log10")+
  labs(colour="Highest Degree attained",
    title="Significant correlation between how targeted the education and average salary.",
      x="Narrowness of path from Education",
       y="Weighted average median salary")+
  theme_minimal()


plotly::ggplotly(plt, tooltip = "text")

